open Lwt.Infix

module Metrics = struct
  open Prometheus

  let namespace = "ocluster"
  let subsystem = "pool"

  let jobs_submitted =
    let help = "Number of jobs submitted to the pool" in
    Counter.v_label ~label_name:"pool" ~help ~namespace ~subsystem "jobs_submitted_total"

  let jobs_cancelled =
    let help = "Number of jobs cancelled while queued" in
    Counter.v_label ~label_name:"pool" ~help ~namespace ~subsystem "jobs_cancelled_total"

  let jobs_accepted =
    let help = "Number of jobs accepted by workers" in
    Counter.v_label ~label_name:"pool" ~help ~namespace ~subsystem "jobs_accepted_total"

  let workers_connected =
    let help = "Number of connected workers" in
    Gauge.v_label ~label_name:"pool" ~help ~namespace ~subsystem "workers_connected"

  let incoming_queue = "incoming"

  let queue_length =
    let help = "Items in the queue" in
    Gauge.v_labels ~label_names:["queue"; "pool"; "priority"] ~help ~namespace ~subsystem "queue_length"

  let workers_ready =
    let help = "Number of workers ready to accept a new job" in
    Gauge.v_label ~label_name:"pool" ~help ~namespace ~subsystem "workers_ready"

  let workers_paused =
    let help = "Number of workers set to inactive" in
    Gauge.v_label ~label_name:"pool" ~help ~namespace ~subsystem "workers_paused"
end

module Dao = struct
  type t = {
    query_cache : Sqlite3.stmt;
    mark_cached : Sqlite3.stmt;
    dump_cache : Sqlite3.stmt;
  }

  let dump f (db, pool) =
    let pp_workers f = function
      | Sqlite3.Data.[ TEXT worker ] -> Fmt.string f worker
      | row -> Fmt.failwith "Bad row from DB: %a" Db.dump_row row
    in
    let first = ref true in
    Db.query db.dump_cache [ TEXT pool ]
    |> List.iter (function
        | Sqlite3.Data.[ TEXT cache_hint ] ->
          if !first then first := false else Fmt.comma f ();
          let workers = Db.query db.query_cache Sqlite3.Data.[ TEXT pool; TEXT cache_hint ] in
          Fmt.pf f "%s: %a" cache_hint (Fmt.Dump.list pp_workers) workers
        | row -> Fmt.failwith "Bad row from DB: %a" Db.dump_row row
      )

  let query_cache t ~pool ~hint =
    Db.query t.query_cache Sqlite3.Data.[ TEXT pool; TEXT hint ]
    |> List.map (function
        | Sqlite3.Data.[ TEXT worker ] -> worker
        | row -> Fmt.failwith "Bad row from DB: %a" Db.dump_row row
      )

  let mark_cached t ~pool ~hint ~worker =
    Db.exec t.mark_cached Sqlite3.Data.[ TEXT pool; TEXT hint; TEXT worker ]

  let init db =
    Sqlite3.exec db "CREATE TABLE IF NOT EXISTS cached ( \
                     pool       TEXT NOT NULL, \
                     cache_hint TEXT NOT NULL, \
                     worker     TEXT NOT NULL, \
                     created    DATETIME NOT NULL, \
                     PRIMARY KEY (pool, cache_hint, worker))" |> Db.or_fail ~cmd:"create table";
    let query_cache = Sqlite3.prepare db "SELECT worker FROM cached WHERE pool = ? AND cache_hint = ? ORDER BY worker" in
    let mark_cached = Sqlite3.prepare db "INSERT OR REPLACE INTO cached (pool, cache_hint, worker, created) VALUES (?, ?, ?, date('now'))" in
    let dump_cache = Sqlite3.prepare db "SELECT DISTINCT cache_hint FROM cached WHERE pool = ? ORDER BY cache_hint" in
    { query_cache; mark_cached; dump_cache }
end

module Make (Item : S.ITEM) = struct
  module Worker_map = Astring.String.Map

  type ticket = {
    item : Item.t;
    urgent : bool;
    mutable cost : int;                         (* Estimated cost (set when added to worker queue) *)
    mutable cancel : (unit -> unit) option;     (* None if not in a queue *)
  }

  let pp_ticket f ticket = Item.pp f ticket.item

  let set_cancel ticket c =
    assert (ticket.cancel = None);
    ticket.cancel <- Some c

  let clear_cancel ticket =
    assert (ticket.cancel <> None);
    ticket.cancel <- None

  let cancel ticket =
    match ticket.cancel with
    | None -> Error `Not_queued
    | Some fn ->
      Log.info (fun f -> f "Cancel %a" pp_ticket ticket);
      ticket.cancel <- None;
      fn ();
      Ok ()

  module Backlog = struct
    type t = {
      queue : string;           (* For metric reports *)
      high : ticket Lwt_dllist.t;
      low : ticket Lwt_dllist.t;
    }

    let choose_queue ~ticket ~pool t =
      let queue, priority =
        match ticket.urgent with
        | true -> t.high, "high"
        | false -> t.low, "low"
      in
      queue, Prometheus.Gauge.labels Metrics.queue_length [t.queue; pool; priority]

    let cancel ?(on_cancel=ignore) ~metric ~pool node () =
      Lwt_dllist.remove node;
      Prometheus.Counter.inc_one (Metrics.jobs_cancelled pool);
      Prometheus.Gauge.dec_one metric;
      on_cancel ()

    let enqueue ?on_cancel ~pool ticket t =
      let queue, metric = choose_queue ~ticket ~pool t in
      let node = Lwt_dllist.add_l ticket queue in
      set_cancel ticket (cancel ?on_cancel ~metric ~pool node);
      Prometheus.Gauge.inc_one metric

    let push_back ~pool ticket t =
      let queue, metric = choose_queue ~ticket ~pool t in
      let node = Lwt_dllist.add_r ticket queue in
      set_cancel ticket (cancel ~metric ~pool node);
      Prometheus.Gauge.inc_one metric

    let dequeue_opt ~pool t =
      let take ticket =
        let priority = if ticket.urgent then "high" else "low" in
        Prometheus.Gauge.dec_one (Prometheus.Gauge.labels Metrics.queue_length [t.queue; pool; priority]);
        clear_cancel ticket;
        Some ticket
      in
      match Lwt_dllist.take_opt_r t.high with
      | Some ticket -> take ticket
      | None ->
        match Lwt_dllist.take_opt_r t.low with
        | Some ticket -> take ticket
        | None -> None

    let length t =
      Lwt_dllist.length t.low + Lwt_dllist.length t.high

    let is_empty t =
      length t = 0

    let create ~queue () =
      {
        queue;
        low = Lwt_dllist.create ();
        high = Lwt_dllist.create ();
      }
  end

  type t = {
    pool : string;                          (* For metrics reporting and DB *)
    db : Dao.t;
    mutable main : [
      | `Backlog of Backlog.t               (* No workers are ready *)
      | `Ready of worker Lwt_dllist.t       (* No work is available. *)
    ];
    mutable workers : worker Worker_map.t;  (* Connected workers *)
    mutable cluster_capacity : float;
  } and worker = {
    parent : t;
    name : string;
    capacity : int;
    mutable state : [ `Running of Backlog.t * unit Lwt_condition.t
                    | `Inactive of unit Lwt.t * unit Lwt.u  (* ready/set_ready for resume *)
                    | `Finished ];
    mutable workload : int;     (* Total cost of items in worker's queue. *)
    mutable shutdown : bool;    (* Worker is paused because it is shutting down. *)
  }

  let enqueue_node item queue metric =
    let node = Lwt_dllist.add_l item queue in
    Prometheus.Gauge.inc_one metric;
    node

  let enqueue_item queue worker ticket =
    let pool = worker.parent.pool in
    let cost = ticket.cost in
    worker.workload <- worker.workload + cost;
    Backlog.enqueue ~pool ticket queue
        ~on_cancel:(fun () -> worker.workload <- worker.workload - cost)

  let dequeue_opt queue worker =
    let pool = worker.parent.pool in
    let ticket = Backlog.dequeue_opt ~pool queue in
    Option.iter (fun ticket -> worker.workload <- worker.workload - ticket.cost) ticket;
    ticket

  (* Return the worker in [workers] with the lowest workload. *)
  let best_worker ~max_workload t workers =
    let rec aux ~best = function
      | [] -> best
      | x :: xs ->
        match Worker_map.find_opt x t.workers with
        | None -> aux ~best xs      (* Worker is not on-line *)
        | Some w when w.workload > max_workload -> aux ~best xs
        | Some w ->
          match w.state with
          | `Inactive _ | `Finished -> aux ~best xs       (* Worker is unavailable *)
          | `Running (q, c) ->
            match best with
            | Some (best_worker, _, _) when best_worker.workload < w.workload -> aux ~best xs
            | _ -> aux xs ~best:(Some (w, q, c))    (* New best worker *)
    in
    aux ~best:None workers

  (* A worker is available for this item, but perhaps there is some other
     worker that should get it instead? e.g. that worker already has part of
     the work cached and will be able to get to it fairly soon. *)
  let assign_preferred t ticket =
    let hint = Item.cache_hint ticket.item in
    let cost = Item.cost_estimate ticket.item in
    if (hint :> string) = "" then false (* Not using cache hints for this item *)
    else (
      let best =
        (* If a worker already has this cached, send it there: *)
        Dao.query_cache t.db ~pool:t.pool ~hint:(hint :> string)
        |> best_worker t ~max_workload:cost.non_cached
      in
      match best with
      | None -> false   (* No workers will have this cached. *)
      | Some (worker, worker_q, cond) ->
        Log.info (fun f -> f "Assigning %a to %S (preferred)" pp_ticket ticket worker.name);
        ticket.cost <- cost.cached;
        enqueue_item worker_q worker ticket;
        Lwt_condition.broadcast cond ();
        true
    )

  let mark_cached item worker =
    let t = worker.parent in
    let hint = Item.cache_hint item in
    Dao.mark_cached t.db ~pool:t.pool ~hint:(hint :> string) ~worker:worker.name

  let pop worker =
    let t = worker.parent in
    let rec aux () =
      match worker.state with
      | `Finished -> Lwt_result.fail `Finished
      | `Inactive (ready, _) -> ready >>= aux
      | `Running (queue, cond) ->
        (* Check our local queue, in case something has already been assigned to us. *)
        match dequeue_opt queue worker with
        | Some ticket ->
          let item = ticket.item in
          Log.info (fun f -> f "%S takes %a from its local queue" worker.name Item.pp item);
          mark_cached ticket.item worker;
          Prometheus.Counter.inc_one (Metrics.jobs_accepted t.pool);
          Lwt_result.return ticket.item
        | None ->
          (* Try the global queue instead. *)
          match t.main with
          | `Ready q ->
            (* No work available. Wait until something arrives. *)
            Log.info (fun f -> f "%S is waiting for more work" worker.name);
            let node = enqueue_node worker q (Metrics.workers_ready t.pool) in
            Lwt_condition.wait cond >>= fun () ->
            Prometheus.Gauge.dec_one (Metrics.workers_ready t.pool);
            Lwt_dllist.remove node; (* In case the wake-up was due to exiting. *)
            aux ()
          | `Backlog q ->
            match Backlog.dequeue_opt ~pool:t.pool q with
            | None ->
              (* Backlog is actually empty. Flip to ready mode and retry
                 (this will add us to the new queue). *)
              t.main <- `Ready (Lwt_dllist.create ());
              aux ()
            | Some ticket ->
              if assign_preferred t ticket then aux ()
              else (
                let item = ticket.item in
                Log.info (fun f -> f "%S takes %a from the main queue" worker.name Item.pp item);
                mark_cached item worker;
                Prometheus.Counter.inc_one (Metrics.jobs_accepted t.pool);
                Lwt_result.return item
              )
    in
    aux ()

  (* Worker is leaving and system is backlogged. Move the worker's items to the backlog. *)
  let rec push_back worker worker_q q =
    let ticket =
      match Lwt_dllist.take_opt_l worker_q.Backlog.low with
      | Some x -> Some x
      | None -> Lwt_dllist.take_opt_l worker_q.Backlog.high
    in
    match ticket with
    | Some ticket ->
      Log.info (fun f -> f "Pushing %a back on to the main queue" pp_ticket ticket);
      let priority = if ticket.urgent then "high" else "low" in
      Prometheus.Gauge.dec_one @@ Prometheus.Gauge.labels Metrics.queue_length [worker_q.queue; worker.parent.pool; priority];
      worker.workload <- worker.workload - ticket.cost;
      clear_cancel ticket;
      Backlog.push_back ~pool:worker.parent.pool ticket q;
      push_back worker worker_q q
    | None -> ()

  let register t ~name ~capacity =
    if Worker_map.mem name t.workers then Error `Name_taken
    else (
      let ready, set_ready = Lwt.wait () in
      let q = {
        parent = t;
        name;
        state = `Inactive (ready, set_ready);
        workload = 0;
        capacity;
        shutdown = false;
      } in
      t.workers <- Worker_map.add name q t.workers;
      t.cluster_capacity <- t.cluster_capacity +. float capacity;
      Prometheus.Gauge.inc_one (Metrics.workers_connected t.pool);
      Prometheus.Gauge.inc_one (Metrics.workers_paused t.pool);
      Ok q
    )

  let rec add t ticket =
    match t.main with
    | `Backlog q ->
      (* No workers are ready. Add to the backlog. *)
      Backlog.enqueue ticket q ~pool:t.pool;
      Log.info (fun f -> f "Adding %a to the backlog" pp_ticket ticket);
      ()
    | `Ready q when Lwt_dllist.is_empty q ->
      (* Ready workers queue is empty. Flip to backlog case and retry. *)
      t.main <- `Backlog (Backlog.create ~queue:Metrics.incoming_queue ());
      add t ticket
    | `Ready q ->
      if not (assign_preferred t ticket) then (
        (* Don't decrement [Metrics.workers_ready] here. That happens when it wakes up. *)
        let worker = Lwt_dllist.take_r q in
        match worker.state with
        | `Inactive _ | `Finished -> add t ticket    (* Stale queue item. Retry. *)
        | `Running (worker_q, cond) ->
          (* Assign to first worker in queue. The worker can't have it cached since it's
             idle and we couldn't find any free preferred worker. *)
          Log.info (fun f -> f "Assigning %a to %S (the next free worker)" pp_ticket ticket worker.name);
          let cost = Item.cost_estimate ticket.item in
          ticket.cost <- cost.non_cached;
          enqueue_item worker_q worker ticket;
          mark_cached ticket.item worker;
          Lwt_condition.broadcast cond ()
      )

  let submit ~urgent t item =
    Prometheus.Counter.inc_one (Metrics.jobs_submitted t.pool);
    let ticket = { item; urgent; cancel = None; cost = -1 } in
    add t ticket;
    ticket

  let add_items t worker_q worker =
    let rec aux () =
      match dequeue_opt worker_q worker with
      | None -> ()
      | Some ticket ->
        add t ticket;
        aux ()
    in
    aux ()

  let set_inactive w =
    let t = w.parent in
    match w.state with
    | `Finished -> failwith "Queue already closed!"
    | `Inactive _ -> ()
    | `Running (worker_q, cond) ->
      let ready, set_ready = Lwt.wait () in
      w.state <- `Inactive (ready, set_ready);
      let len = Backlog.length worker_q in
      Log.info (fun f -> f "%S marked inactive (reassigning %d items)" w.name len);
      Prometheus.Gauge.inc_one (Metrics.workers_paused w.parent.pool);
      begin
        if Backlog.is_empty worker_q then ()
        else match t.main with
          | `Backlog q ->
            (* Push our items back onto the existing backlog. *)
            push_back w worker_q q
          | `Ready _ ->
            (* The main queue is empty, so just add the items as normal. *)
            add_items t worker_q w
      end;
      Lwt_condition.broadcast cond ()   (* Wake the worker's pop thread. *)

  let set_active w = function
    | false -> set_inactive w
    | true when w.shutdown ->
      Log.info (fun f -> f "Ignoring request to activate %S as it is shutting down" w.name);
    | true ->
      match w.state with
      | `Finished -> failwith "Queue already closed!"
      | `Running _ -> ()
      | `Inactive (_, set_ready) ->
        Log.info (fun f -> f "Activate queue for %S" w.name);
        Prometheus.Gauge.dec_one (Metrics.workers_paused w.parent.pool);
        w.state <- `Running (Backlog.create ~queue:w.name (), Lwt_condition.create ());
        Lwt.wakeup set_ready ()

  let shutdown w =
    Log.info (fun f -> f "Worker %S is shutting down" w.name);
    w.shutdown <- true;
    set_active w false

  (* Worker [w] has disconnected. *)
  let release w =
    set_inactive w;
    Log.info (fun f -> f "Release worker %S" w.name);
    match w.state with
    | `Inactive (_, set_ready) ->
      w.state <- `Finished;
      let t = w.parent in
      t.workers <- Worker_map.remove w.name t.workers;
      t.cluster_capacity <- t.cluster_capacity -. float w.capacity;
      Prometheus.Gauge.dec_one (Metrics.workers_connected t.pool);
      Prometheus.Gauge.dec_one (Metrics.workers_paused t.pool);
      Lwt.wakeup set_ready ()
    | _ -> assert false

  let connected_workers t = t.workers

  let is_active worker =
    match worker.state with
    | `Running _ -> true
    | `Inactive _ | `Finished -> false

  let create ~name ~db =
    {
      pool = name;
      db;
      main = `Backlog (Backlog.create ~queue:Metrics.incoming_queue ());
      workers = Worker_map.empty;
      cluster_capacity = 0.0;
    }

  let dump_queue ?(sep=Fmt.sp) pp f q =
    let first = ref true in
    Fmt.string f "[";
    q |> Lwt_dllist.iter_l (fun item ->
        if !first then first := false else sep f ();
        pp f item
      );
    Fmt.string f "]"

  let pp_worker f worker =
    Fmt.string f worker.name

  let pp_cost_item f ticket =
    let urgent = if ticket.urgent then "+urgent" else "" in
    Fmt.pf f "%a(%d%s)" pp_ticket ticket ticket.cost urgent

  let pp_state f = function
    | { state = `Finished; _ } -> Fmt.string f "(finished)"
    | { shutdown = true; _ } -> Fmt.string f "(shutting down)"
    | { state = `Inactive _; _ } -> Fmt.string f "(inactive)"
    | { state = `Running (q, _); _} ->
      Fmt.pf f "%a : %a"
        (dump_queue pp_cost_item) q.low
        (dump_queue pp_cost_item) q.high

  let dump_workers f x =
    let pp_item f (id, w) =
      Fmt.pf f "@,%s (%d): @[%a@]" id w.workload pp_state w in
    Worker_map.bindings x
    |> Fmt.(list ~sep:nop) pp_item f

  let dump_main f = function
    | `Backlog (q : Backlog.t) ->
      Fmt.pf f "(backlog) %a : %a"
        (dump_queue pp_ticket) q.low
        (dump_queue pp_ticket) q.high
    | `Ready q ->
      Fmt.pf f "(ready) %a" (dump_queue pp_worker) q

  let show f {pool = _; db = _; main; workers; cluster_capacity } =
    Fmt.pf f "@[<v>capacity: %.0f@,queue: @[%a@]@,@[<v2>registered:%a@]@]@."
      cluster_capacity
      dump_main main
      dump_workers workers

  let dump f {pool; db; main; workers; cluster_capacity } =
    Fmt.pf f "@[<v>capacity: %.0f@,queue: @[%a@]@,@[<v2>registered:%a@]@,cached: @[%a@]@]@."
      cluster_capacity
      dump_main main
      dump_workers workers
      Dao.dump (db, pool)
end

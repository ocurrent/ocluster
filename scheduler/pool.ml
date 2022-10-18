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

  let workers_capacity =
    let help = "Aggregate capacity of connected workers" in
    Gauge.v_label ~label_name:"pool" ~help ~namespace ~subsystem "workers_capacity"

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

  let priority ~urgent =
    if urgent then "high" else "low"
end

module Client_map = Map.Make(String)

module Inactive_reasons = struct
  type t = int

  let empty = 0
  let worker = 1
  let admin_pause = 2
  let admin_shutdown = 4

  let mem a b =
    (a land b) = a

  let union = (lor)

  let pp f x =
    if x = empty then Fmt.string f "active"
    else (
      let first = ref true in
      let add msg =
        if !first then first := false
        else Fmt.string f ", ";
        Fmt.string f msg
      in
      if mem worker x then add "worker pause";
      if mem admin_pause x then add "admin pause";
      if mem admin_shutdown x then add "shutting down"
    )
end

module Dao = struct
  type t = {
    query_cache : Sqlite3.stmt;
    mark_cached : Sqlite3.stmt;
    dump_cache : Sqlite3.stmt;
    get_rate : Sqlite3.stmt;
    set_rate : Sqlite3.stmt;
    remove_user : Sqlite3.stmt;
    worker_known : Sqlite3.stmt;
    known_workers : Sqlite3.stmt;
    ensure_worker : Sqlite3.stmt;
    forget_worker : Sqlite3.stmt;
    set_paused : Sqlite3.stmt;
    get_paused : Sqlite3.stmt;
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

  let get_rate t ~pool ~client_id =
    Db.query_some t.get_rate Sqlite3.Data.[ TEXT pool; TEXT client_id ]
    |> Option.map (function
        | Sqlite3.Data.[ FLOAT rate ] -> rate
        | row -> Fmt.failwith "Bad row from DB: %a" Db.dump_row row
      )
    |> Option.value ~default:1.0

  let known_workers t ~pool =
    Db.query t.known_workers Sqlite3.Data.[ TEXT pool ] |> List.map @@ function
    | Sqlite3.Data.[ TEXT worker; INT 1L ] -> worker, true
    | Sqlite3.Data.[ TEXT worker; INT 0L ] -> worker, false
    | row -> Fmt.failwith "Bad row from DB: %a" Db.dump_row row

  let worker_known t ~pool worker =
    match Db.query_one t.worker_known Sqlite3.Data.[ TEXT pool; TEXT worker ] with
    | Sqlite3.Data.[ INT 1L ] -> true
    | Sqlite3.Data.[ INT 0L ] -> false
    | row -> Fmt.failwith "Bad row from DB: %a" Db.dump_row row

  let ensure_worker t ~pool worker =
    Db.exec t.ensure_worker Sqlite3.Data.[ TEXT pool; TEXT worker ]

  let forget_worker t ~pool worker =
    Db.exec t.forget_worker Sqlite3.Data.[ TEXT pool; TEXT worker ]

  let set_paused t ~pool ~worker v =
    let v = Bool.to_int v |> Int64.of_int in
    Db.exec t.set_paused Sqlite3.Data.[ INT v; TEXT pool; TEXT worker ]

  let get_paused t ~pool ~worker =
    match Db.query_one t.get_paused Sqlite3.Data.[ TEXT pool; TEXT worker ] with
    | Sqlite3.Data.[ INT 1L ] -> true
    | Sqlite3.Data.[ INT 0L ] -> false
    | row -> Fmt.failwith "Bad row from DB: %a" Db.dump_row row

  let init db =
    Sqlite3.exec db "CREATE TABLE IF NOT EXISTS cached ( \
                     pool       TEXT NOT NULL, \
                     cache_hint TEXT NOT NULL, \
                     worker     TEXT NOT NULL, \
                     created    DATETIME NOT NULL, \
                     PRIMARY KEY (pool, cache_hint, worker))" |> Db.or_fail ~cmd:"create table";
    Sqlite3.exec db "CREATE TABLE IF NOT EXISTS pool_user_rate ( \
                     pool       TEXT NOT NULL, \
                     user       TEXT NOT NULL, \
                     rate       REAL NOT NULL, \
                     PRIMARY KEY (pool, user))" |> Db.or_fail ~cmd:"create pool_user_rate table";
    Sqlite3.exec db "CREATE TABLE IF NOT EXISTS workers ( \
                     pool       TEXT NOT NULL, \
                     id         TEXT NOT NULL, \
                     paused     BOOLEAN DEFAULT 0, \
                     PRIMARY KEY (pool, id))" |> Db.or_fail ~cmd:"create workers table";
    let query_cache = Sqlite3.prepare db "SELECT worker FROM cached WHERE pool = ? AND cache_hint = ? ORDER BY worker" in
    let mark_cached = Sqlite3.prepare db "INSERT OR REPLACE INTO cached (pool, cache_hint, worker, created) VALUES (?, ?, ?, date('now'))" in
    let dump_cache = Sqlite3.prepare db "SELECT DISTINCT cache_hint FROM cached WHERE pool = ? ORDER BY cache_hint" in
    let set_rate = Sqlite3.prepare db "INSERT OR REPLACE INTO pool_user_rate (pool, user, rate) VALUES (?, ?, ?)" in
    let get_rate = Sqlite3.prepare db "SELECT rate FROM pool_user_rate WHERE pool = ? AND user = ?" in
    let remove_user = Sqlite3.prepare db "DELETE FROM pool_user_rate WHERE pool = ? AND user = ?" in
    let known_workers = Sqlite3.prepare db "SELECT id, paused FROM workers WHERE pool = ?" in
    let worker_known = Sqlite3.prepare db "SELECT EXISTS (SELECT 1 FROM workers WHERE pool = ? AND id = ?)" in
    let ensure_worker = Sqlite3.prepare db "INSERT OR IGNORE INTO workers (pool, id) VALUES (?, ?)" in
    let forget_worker = Sqlite3.prepare db "DELETE FROM workers WHERE pool = ? AND id = ?" in
    let set_paused = Sqlite3.prepare db "UPDATE workers SET paused = ? WHERE pool = ? AND id = ?" in
    let get_paused = Sqlite3.prepare db "SELECT paused FROM workers WHERE pool = ? AND id = ?" in
    { query_cache; mark_cached; dump_cache; set_rate; get_rate; remove_user;
      known_workers; worker_known; ensure_worker; forget_worker; set_paused; get_paused }
end

let (let<>) x f =
  if x = 0 then f ()
  else x

let pp_rough_duration f x =
  if x < 120.0 then
    Fmt.pf f "%.0fs" x
  else
    Fmt.pf f "%.0fm" (x /. 60.)

module Make (Item : S.ITEM)(Time : S.TIME) = struct
  module Worker_map = Map.Make(String)

  type ticket = {
    key : < >;
    item : Item.t;
    fair_start_time : float;
    urgent : bool;
    client_id : string;
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

  module Backlog = struct
    module Key = struct
      type t = < >

      let compare = compare
    end

    module Ticket = struct
      type t = ticket

      let compare a b =
        let<> () = compare b.urgent a.urgent in
        compare a.fair_start_time b.fair_start_time

      let pp_ticket ~now f ticket =
        Fmt.pf f "%s:%a@@%a"
          ticket.client_id
          Item.pp ticket.item
          pp_rough_duration (ticket.fair_start_time -. now)

      let pp ~now f ticket =
        let urgent = if ticket.urgent then "+urgent" else "" in
        if ticket.cost >= 0 then
          Fmt.pf f "%a(%d%s)" (pp_ticket ~now) ticket ticket.cost urgent
        else
          Fmt.pf f "%a%s" (pp_ticket ~now) ticket urgent
    end

    module Q = Psq.Make(Key)(Ticket)

    type t = {
      queue : string;           (* For metric reports *)
      mutable psq : Q.t;
    }

    let cancel t ?(on_cancel=ignore) ~metric ~pool key () =
      t.psq <- Q.remove key t.psq;
      Prometheus.Counter.inc_one (Metrics.jobs_cancelled pool);
      Prometheus.Gauge.dec_one metric;
      on_cancel ()

    let enqueue ?on_cancel ~pool ticket t =
      let metric = Prometheus.Gauge.labels Metrics.queue_length [t.queue; pool; Metrics.priority ~urgent:ticket.urgent] in
      t.psq <- Q.add ticket.key ticket t.psq;
      set_cancel ticket (cancel t ?on_cancel ~metric ~pool ticket.key);
      Prometheus.Gauge.inc_one metric

    let dequeue_opt ~pool t =
      match Q.pop t.psq with
      | None -> None
      | Some ((_key, ticket), q) ->
        t.psq <- q;
        let priority = if ticket.urgent then "high" else "low" in
        Prometheus.Gauge.dec_one (Prometheus.Gauge.labels Metrics.queue_length [t.queue; pool; priority]);
        clear_cancel ticket;
        Some ticket

    let length t = Q.size t.psq

    let is_empty t =
      length t = 0

    let dump f t =
      let items = Q.to_priority_list t.psq |> List.rev in
      let now = Time.gettimeofday () in
      Fmt.pf f "[@[<hov>%a@]]"
        (Fmt.(list ~sep:sp) (Fmt.using snd (Ticket.pp ~now))) items

    let create ~queue () =
      {
        queue;
        psq = Q.empty;
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
    mutable clients : client_info Client_map.t;
    mutable cluster_capacity : float;
    pending_cached : (Item.cache_hint, int) Hashtbl.t;
  } and worker = {
    parent : t;
    name : string;
    capacity : int;
    mutable state : [ `Running of Backlog.t * unit Lwt_condition.t
                    | `Inactive of Inactive_reasons.t * unit Lwt.t * unit Lwt.u  (* ready/set_ready for resume *)
                    | `Finished ];
    mutable workload : int;     (* Total cost of items in worker's queue. *)
    mutable running : int;      (* Number of jobs currently running. *)
    job_finished_cond : unit Lwt_condition.t;
  } and client_info = {
    id : string;
    mutable next_fair_start_time : float;
    mutable finished : bool;
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

  let dec_pending_count t ticket =
    let hint = Item.cache_hint ticket.item in
    if (hint :> string) <> "" then (
      let pending_count = Hashtbl.find t.pending_cached hint in
      if pending_count > 1 then
        Hashtbl.replace t.pending_cached hint (pending_count - 1)
      else
        Hashtbl.remove t.pending_cached hint
    )

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
      | `Inactive (_, ready, _) -> ready >>= aux
      | `Running (queue, cond) ->
        (* Check our local queue, in case something has already been assigned to us. *)
        match dequeue_opt queue worker with
        | Some ticket ->
          let item = ticket.item in
          Log.info (fun f -> f "%S takes %a from its local queue" worker.name Item.pp item);
          mark_cached ticket.item worker;
          worker.running <- worker.running + 1;
          Prometheus.Counter.inc_one (Metrics.jobs_accepted t.pool);
          dec_pending_count t ticket;
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
                worker.running <- worker.running + 1;
                Prometheus.Counter.inc_one (Metrics.jobs_accepted t.pool);
                dec_pending_count t ticket;
                Lwt_result.return item
              )
    in
    aux ()

  let job_finished worker =
    Log.debug (fun f -> f "%S finishes a job" worker.name);
    worker.running <- worker.running - 1;
    Lwt_condition.broadcast worker.job_finished_cond ()

  let rec running_jobs ?(prev=(-1)) worker =
    if worker.running <> prev then Lwt.return worker.running
    else (
      Lwt_condition.wait worker.job_finished_cond >>= fun () ->
      running_jobs ~prev worker
    )

  (* Worker is leaving and system is backlogged. Move the worker's items to the backlog. *)
  let rec push_back worker worker_q q =
    match Backlog.dequeue_opt ~pool:worker.parent.pool worker_q with
    | None -> ()
    | Some ticket ->
      Log.info (fun f -> f "Pushing %a back on to the main queue" pp_ticket ticket);
      worker.workload <- worker.workload - ticket.cost;
      ticket.cost <- -1;
      Backlog.enqueue ~pool:worker.parent.pool ticket q;
      push_back worker worker_q q

  let register t ~name ~capacity =
    if Worker_map.mem name t.workers then Error `Name_taken
    else (
      let ready, set_ready = Lwt.wait () in
      Dao.ensure_worker t.db ~pool:t.pool name;
      let inactive_reasons =
        Inactive_reasons.(union worker) (
          if Dao.get_paused t.db ~pool:t.pool ~worker:name then Inactive_reasons.admin_pause
          else Inactive_reasons.empty
        )
      in
      let q = {
        parent = t;
        name;
        state = `Inactive (inactive_reasons, ready, set_ready);
        workload = 0;
        running = 0;
        job_finished_cond = Lwt_condition.create ();
        capacity;
      } in
      t.workers <- Worker_map.add name q t.workers;
      t.cluster_capacity <- t.cluster_capacity +. float capacity;
      Prometheus.Gauge.inc_one (Metrics.workers_connected t.pool);
      Prometheus.Gauge.inc (Metrics.workers_capacity t.pool) (float capacity);
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

  let add_items t worker_q worker =
    let rec aux () =
      match dequeue_opt worker_q worker with
      | None -> ()
      | Some ticket ->
        ticket.cost <- -1;
        add t ticket;
        aux ()
    in
    aux ()

  let set_inactive ~reason w =
    let t = w.parent in
    match w.state with
    | `Finished -> failwith "Queue already closed!"
    | `Inactive (reasons, r, sr) ->
      w.state <- `Inactive (Inactive_reasons.union reasons reason, r, sr)
    | `Running (worker_q, cond) ->
      let ready, set_ready = Lwt.wait () in
      w.state <- `Inactive (reason, ready, set_ready);
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

  let set_active ~reason w active =
    assert (reason <> Inactive_reasons.empty);
    if Inactive_reasons.(mem admin_pause) reason then
      Dao.set_paused w.parent.db ~pool:w.parent.pool ~worker:w.name (not active);
    match active with
    | false -> set_inactive ~reason w
    | true ->
      match w.state with
      | `Finished -> failwith "Queue already closed!"
      | `Running _ -> ()
      | `Inactive (reasons, _, set_ready) when Inactive_reasons.mem reasons reason ->
        Log.info (fun f -> f "Activate queue for %S" w.name);
        Prometheus.Gauge.dec_one (Metrics.workers_paused w.parent.pool);
        w.state <- `Running (Backlog.create ~queue:w.name (), Lwt_condition.create ());
        Lwt.wakeup set_ready ()
      | `Inactive (reasons, ready, set_ready) ->
        if Inactive_reasons.mem reason reasons then (
          let reasons = reasons - reason in
          Log.info (fun f -> f "Removed inactive reason %a for %S (now %a)"
                       Inactive_reasons.pp reason
                       w.name
                       Inactive_reasons.pp reasons);
          w.state <- `Inactive (reasons, ready, set_ready)
        )

  let shutdown w =
    Log.info (fun f -> f "Worker %S is shutting down" w.name);
    set_active w false ~reason:Inactive_reasons.admin_shutdown

  (* Worker [w] has disconnected. *)
  let release w =
    set_inactive ~reason:Inactive_reasons.worker w;
    Log.info (fun f -> f "Release worker %S" w.name);
    match w.state with
    | `Inactive (_, _, set_ready) ->
      w.state <- `Finished;
      let t = w.parent in
      t.workers <- Worker_map.remove w.name t.workers;
      t.cluster_capacity <- t.cluster_capacity -. float w.capacity;
      Prometheus.Gauge.dec_one (Metrics.workers_connected t.pool);
      Prometheus.Gauge.dec (Metrics.workers_capacity t.pool) (float w.capacity);
      Prometheus.Gauge.dec_one (Metrics.workers_paused t.pool);
      Lwt.wakeup set_ready ()
    | _ -> assert false

  let connected_workers t = t.workers

  let with_worker t name f =
    match register t ~name ~capacity:0 with
    | Error `Name_taken -> f (Worker_map.find name t.workers)
    | Ok w ->
      Fun.protect (fun () -> f w)
        ~finally:(fun () -> release w)

  let worker_known t name =
    Dao.worker_known t.db ~pool:t.pool name

  let forget_worker t name =
    if worker_known t name then (
      if Worker_map.mem name t.workers then Error `Still_connected
      else (
        Dao.forget_worker t.db ~pool:t.pool name;
        Ok ()
      )
    ) else Error `Unknown_worker

  let inactive_reasons worker =
    match worker.state with
    | `Running _ -> Inactive_reasons.empty
    | `Inactive (reasons, _, _) -> reasons
    | `Finished -> Inactive_reasons.worker

  let is_active worker =
    inactive_reasons worker = Inactive_reasons.empty

  let known_workers t =
    Dao.known_workers t.db ~pool:t.pool
    |> List.map (fun (name, paused) ->
        let connected, active =
          match Worker_map.find_opt name t.workers with
          | None -> false, not paused
          | Some w -> true, is_active w
        in
        { Cluster_api.Pool_admin.name; active; connected }
      )

  let create ~name ~db =
    {
      pool = name;
      db;
      main = `Backlog (Backlog.create ~queue:Metrics.incoming_queue ());
      clients = Client_map.empty;
      workers = Worker_map.empty;
      cluster_capacity = 0.0;
      pending_cached = Hashtbl.create 1024;
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

  let pp_state f = function
    | { state = `Finished; _ } -> Fmt.string f "(finished)"
    | { state = `Inactive (reasons, _, _); _ } -> Fmt.pf f "(inactive: %a)" Inactive_reasons.pp reasons
    | { state = `Running (q, _); _} -> Backlog.dump f q

  let dump_workers f x =
    let pp_item f (id, w) =
      Fmt.pf f "@,%s (%d): @[%a@] (%d running)" id w.workload pp_state w w.running in
    Worker_map.bindings x
    |> Fmt.(list ~sep:nop) pp_item f

  let dump_disconnected =
    let pp_item f (id, paused) =
      Fmt.pf f "@,%s" id;
      if paused then Fmt.string f " (paused)"
    in
    Fmt.(list ~sep:nop) pp_item

  let dump_main f = function
    | `Backlog (q : Backlog.t) ->
      Fmt.pf f "(backlog) %a" Backlog.dump q
    | `Ready q ->
      Fmt.pf f "(ready) %a" (dump_queue pp_worker) q

  let dump_client t ~now f (_, { id; next_fair_start_time; finished }) =
    let delay = next_fair_start_time -. now in
    if finished then Fmt.pf f "%s:FINISHED" id
    else (
      let rate = Dao.get_rate ~pool:t.pool ~client_id:id t.db in
      let pp_delay f x =
        if delay > 0.0 then Fmt.pf f "+%a" pp_rough_duration x
      in
      Fmt.pf f "%s(%.0f)%a" id rate pp_delay delay
    )

  let dump_clients t f clients =
    let now = Time.gettimeofday () in
    Fmt.(seq ~sep:sp (dump_client t ~now)) f (Client_map.to_seq clients)

  let pp_common f ({pool; db; main; clients; workers; cluster_capacity; pending_cached = _} as t) =
    let disconnected =
      Dao.known_workers db ~pool
      |> List.filter_map (fun (name, paused) ->
          if Worker_map.mem name workers then None
          else Some (name, paused)
        )
    in
    Fmt.pf f "capacity: %.0f@,queue: @[%a@]@,@[<v2>registered:%a@]@,@[<v2>disconnected:%a@]@,clients: @[<hov>%a@]"
      cluster_capacity
      dump_main main
      dump_workers workers
      dump_disconnected disconnected
      (dump_clients t) clients

  let show f t =
    Fmt.pf f "@[<v>%a@]@." pp_common t

  let dump f t =
    Fmt.pf f "@[<v>%a@,cached: @[%a@]@]@."
      pp_common t
      Dao.dump (t.db, t.pool)

  module Client = struct
    type nonrec t = {
      parent : t;
      info : client_info;
    }

    let set_rate (t:t) rate =
      let pool = t.parent in
      assert (rate > 0.0);
      Db.exec pool.db.set_rate Sqlite3.Data.[ TEXT pool.pool; TEXT t.info.id; FLOAT rate ]

    let get_rate (t:t) =
      let pool = t.parent in
      Dao.get_rate ~pool:pool.pool ~client_id:t.info.id pool.db

    let schedule t cost =
      let rate = get_rate t in
      let start = max (Time.gettimeofday ()) t.info.next_fair_start_time in
      t.info.next_fair_start_time <- start +. (cost /. rate);
      start

    let submit ~urgent (t:t) item =
      assert (not t.info.finished);
      let pool = t.parent in
      let cost =
        let costs = Item.cost_estimate item in
        let hint = Item.cache_hint item in
        if (hint :> string) = "" then costs.non_cached
        else (
          let pending_count = Hashtbl.find_opt pool.pending_cached hint |> Option.value ~default:0 in
          Hashtbl.replace pool.pending_cached hint (pending_count + 1);
          if pending_count > 0 || Dao.query_cache pool.db ~pool:pool.pool ~hint:(hint :> string) <> [] then costs.cached
          else costs.non_cached
        )
      in
      let fair_start_time = schedule t (float cost) in
      Prometheus.Counter.inc_one (Metrics.jobs_submitted pool.pool);
      let key = object end in
      let ticket = { key; item; client_id = t.info.id; fair_start_time; urgent; cancel = None; cost = -1 } in
      add pool ticket;
      ticket

    let cancel (t:t) ticket =
      assert (not t.info.finished);
      match ticket.cancel with
      | None -> Error `Not_queued
      | Some fn ->
        Log.info (fun f -> f "Cancel %a" pp_ticket ticket);
        dec_pending_count t.parent ticket;
        ticket.cancel <- None;
        fn ();
        Ok ()

    let client_id t = t.info.id
    let pool_id t = t.parent.pool

    let v parent info = { parent; info }
  end

  let client t ~client_id =
    let info =
      match Client_map.find_opt client_id t.clients with
      | Some c -> c
      | None ->
        let info = {
          id = client_id;
          next_fair_start_time = Time.gettimeofday ();
          finished = false;
        } in
        t.clients <- Client_map.add client_id info t.clients;
        info
    in
    Client.v t info

  let remove_client t ~client_id =
    Db.exec t.db.remove_user Sqlite3.Data.[ TEXT t.pool; TEXT client_id ];
    Client_map.find_opt client_id t.clients
    |> Option.iter @@ fun client ->
    client.finished <- true;
    t.clients <- Client_map.remove client_id t.clients
end

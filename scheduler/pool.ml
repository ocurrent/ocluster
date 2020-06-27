open Lwt.Infix

module Metrics = struct
  open Prometheus

  let namespace = "scheduler"
  let subsystem = "pool"

  let workers_connected =
    let help = "Number of connected workers" in
    Gauge.v_label ~label_name:"pool" ~help ~namespace ~subsystem "workers_connected"

  let incoming_queue_length =
    let help = "Items in the main queue (i.e. unassigned)" in
    Gauge.v_label ~label_name:"pool" ~help ~namespace ~subsystem "incoming_queue_length"

  let assigned_items =
    let help = "Number of items assigned to a particular worker" in
    Gauge.v_label ~label_name:"worker" ~help ~namespace ~subsystem "assigned_items"

  let workers_ready =
    let help = "Number of workers ready to accept a new job" in
    Gauge.v_label ~label_name:"pool" ~help ~namespace ~subsystem "workers_ready"
end

module Make (Item : S.ITEM) = struct
  type worker_id = string

  type t = {
    pool : string;                        (* For metrics reporting *)
    mutable main : [
      | `Backlog of Item.t Lwt_dllist.t   (* No workers are ready *)
      | `Ready of worker Lwt_dllist.t     (* No work is available. *)
    ];
    workers : (worker_id, worker) Hashtbl.t;
    cached : (Item.cache_hint, worker_id list) Hashtbl.t;     (* todo: this should go in a database *)
    will_cache : (Item.cache_hint, worker_id list) Hashtbl.t; (* Which workers will soon have cached this? *)
  } and worker = {
    parent : t;
    name : string;
    mutable state : [ `Running of (int * Item.t) Lwt_dllist.t * unit Lwt_condition.t | `Finished ];
    mutable workload : int;     (* Total cost of items in worker's queue. *)
  }

  let enqueue_node item queue metric =
    let node = Lwt_dllist.add_l item queue in
    Prometheus.Gauge.inc_one metric;
    node

  let enqueue item queue metric =
    let _ : _ Lwt_dllist.node = enqueue_node item queue metric in
    ()

  let dequeue queue metric =
    let item = Lwt_dllist.take_r queue in
    Prometheus.Gauge.dec_one metric;
    item

  let dequeue_opt queue metric =
    match Lwt_dllist.take_opt_r queue with
    | None -> None
    | Some _ as item ->
      Prometheus.Gauge.dec_one metric;
      item

  (* Return the worker in [workers] with the lowest workload. *)
  let best_worker ~max_workload t workers =
    let rec aux ~best = function
      | [] -> best
      | x :: xs ->
        match Hashtbl.find_opt t.workers x with
        | None -> aux ~best xs      (* Worker is not on-line *)
        | Some w when w.workload > max_workload -> aux ~best xs
        | Some w ->
          match w.state with
          | `Finished -> aux ~best xs       (* Worker is shutting down *)
          | `Running (q, c) ->
            match best with
            | Some (best_worker, _, _) when best_worker.workload < w.workload -> aux ~best xs
            | _ -> aux xs ~best:(Some (w, q, c))    (* New best worker *)
    in
    aux ~best:None workers

  (* A worker is available for this item, but perhaps there is some other
     worker that should get it instead? e.g. that worker already has part of
     the work cached and will be able to get to it fairly soon. *)
  let assign_preferred t item =
    let hint = Item.cache_hint item in
    let cost = Item.cost_estimate item in
    if (hint :> string) = "" then false (* Not using cache hints for this item *)
    else (
      let best =
        (* If a worker already has this cached, send it there: *)
        match Option.bind (Hashtbl.find_opt t.cached hint) (best_worker t ~max_workload:cost.non_cached) with
        | Some best -> Some best
        | None ->
          (* If not, maybe some worker will have it cached soon: *)
          Option.bind (Hashtbl.find_opt t.will_cache hint) (best_worker t ~max_workload:cost.non_cached)
      in
      match best with
      | None -> false   (* No workers will have this cached. *)
      | Some (worker, worker_q, cond) ->
        Log.info (fun f -> f "Assigning %a to %S (preferred)" Item.pp item worker.name);
        enqueue (cost.cached, item) worker_q (Metrics.assigned_items worker.name);
        worker.workload <- worker.workload + cost.cached;
        Lwt_condition.broadcast cond ();
        true
    )

  let clear_will_cache item worker =
    let t = worker.parent in
    let hint = Item.cache_hint item in
    match Hashtbl.find_opt t.will_cache hint with
    | None -> ()
    | Some ids ->
      match List.filter ((<>) worker.name) ids with
      | [] -> Hashtbl.remove t.will_cache hint
      | ids -> Hashtbl.replace t.will_cache hint ids

  let mark_cached item worker =
    clear_will_cache item worker;
    let t = worker.parent in
    let hint = Item.cache_hint item in
    let prev = Hashtbl.find_opt t.cached hint |> Option.value ~default:[] in
    if not (List.mem worker.name prev) then
      Hashtbl.replace t.cached hint (worker.name :: prev)

  let pop worker =
    let t = worker.parent in
    let rec aux () =
      match worker.state with
      | `Finished -> Lwt_result.fail `Finished
      | `Running (queue, cond) ->
        (* Check our local queue, in case something has already been assigned to us. *)
        match dequeue_opt queue (Metrics.assigned_items worker.name) with
        | Some (cost, item) ->
          Log.info (fun f -> f "%S takes %a from its local queue" worker.name Item.pp item);
          worker.workload <- worker.workload - cost;
          mark_cached item worker;
          Lwt_result.return item
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
            match dequeue_opt q (Metrics.incoming_queue_length t.pool) with
            | None ->
              (* Backlog is actually empty. Flip to ready mode and retry
                 (this will add us to the new queue). *)
              t.main <- `Ready (Lwt_dllist.create ());
              aux ()
            | Some item ->
              if assign_preferred t item then aux ()
              else (
                Log.info (fun f -> f "%S takes %a from the main queue" worker.name Item.pp item);
                mark_cached item worker;
                Lwt_result.return item
              )
    in
    aux ()

  (* Worker is leaving and system is backlogged. Move the worker's items to the backlog.
     Metrics have already been updated. *)
  let rec push_back worker worker_q q =
    match Lwt_dllist.take_opt_l worker_q with
    | Some (_cost, item) ->
      Log.info (fun f -> f "Pushing %a back on to the main queue" Item.pp item);
      let _ : _ Lwt_dllist.node = Lwt_dllist.add_r item q in
      clear_will_cache item worker;
      push_back worker worker_q q
    | None -> ()

  let register t ~name =
    if Hashtbl.mem t.workers name then Error `Name_taken
    else (
      let q = {
        parent = t;
        name;
        state = `Running (Lwt_dllist.create (), Lwt_condition.create ());
        workload = 0;
      } in
      Hashtbl.add t.workers name q;
      Prometheus.Gauge.inc_one (Metrics.workers_connected t.pool);
      Ok q
    )

  let rec submit t item =
    match t.main with
    | `Backlog q ->
      (* No workers are ready. Add to the backlog. *)
      enqueue item q (Metrics.incoming_queue_length t.pool);
      Log.info (fun f -> f "Adding %a to the backlog" Item.pp item);
      ()
    | `Ready q when Lwt_dllist.is_empty q ->
      (* Ready workers queue is empty. Flip to backlog case and retry. *)
      t.main <- `Backlog (Lwt_dllist.create ());
      submit t item
    | `Ready q ->
      if not (assign_preferred t item) then (
        (* Don't decrement [Metrics.workers_ready] here. That happens when it wakes up. *)
        let worker = Lwt_dllist.take_r q in
        match worker.state with
        | `Finished -> submit t item    (* Stale queue item. Retry. *)
        | `Running (worker_q, cond) ->
          (* Assign to first worker in queue. The worker can't have it cached since it's
             idle and we couldn't find any free preferred worker. *)
          Log.info (fun f -> f "Assigning %a to %S (the next free worker)" Item.pp item worker.name);
          let cost = Item.cost_estimate item in
          let cache_hint = Item.cache_hint item in
          let prev = Hashtbl.find_opt t.will_cache cache_hint |> Option.value ~default:[] in
          Hashtbl.replace t.will_cache cache_hint (worker.name :: prev);
          enqueue (cost.non_cached, item) worker_q (Metrics.assigned_items worker.name);
          worker.workload <- worker.workload + cost.non_cached;
          Lwt_condition.broadcast cond ()
      )

  (* Worker [w] has disconnected. *)
  let release w =
    let t = w.parent in
    let assigned_items = Metrics.assigned_items w.name in
    match w.state with
    | `Finished -> failwith "Queue already closed!"
    | `Running (worker_q, cond) ->
      w.state <- `Finished;
      Hashtbl.remove t.workers w.name;
      Prometheus.Gauge.dec_one (Metrics.workers_connected t.pool);
      let rec reassign () =
        if Lwt_dllist.is_empty worker_q then ()
        else match t.main with
          | `Backlog q ->
            (* Push our items back onto the existing backlog. *)
            let n = float_of_int (Lwt_dllist.length worker_q) in
            Prometheus.Gauge.dec assigned_items n;
            Prometheus.Gauge.inc (Metrics.incoming_queue_length t.pool) n;
            push_back w worker_q q
          | `Ready _ ->
            (* The main queue is empty, so just add the items as normal. *)
            let (_old_cost, item) = dequeue worker_q assigned_items in
            submit t item;
            reassign ()
      in
      let len = Lwt_dllist.length worker_q in
      Log.info (fun f -> f "%S disconnected (reassigning %d items)" w.name len);
      reassign ();
      Lwt_condition.broadcast cond ()   (* Wake the worker's pop thread. *)

  let create ~name =
    {
      pool = name;
      main = `Backlog (Lwt_dllist.create ());
      workers = Hashtbl.create 10;
      cached = Hashtbl.create 1000;
      will_cache = Hashtbl.create 1000;
    }

  let dump_queue ?(sep=Fmt.cut) pp f q =
    let first = ref true in
    Fmt.string f "[";
    q |> Lwt_dllist.iter_l (fun item ->
        if !first then first := false else sep f ();
        pp f item
      );
    Fmt.string f "]"

  let pp_worker f worker =
    Fmt.string f worker.name

  let pp_cost_item f (cost, item) =
    Fmt.pf f "%a(%d)" Item.pp item cost

  let pp_state f = function
    | `Finished -> Fmt.string f "(finished)"
    | `Running (q, _) -> dump_queue ~sep:Fmt.sp pp_cost_item f q

  let dump_workers =
    let pp_item f (id, w) =
      Fmt.pf f "@,%s (%d): @[%a@]" id w.workload pp_state w.state in
    Fmt.(hashtbl ~sep:nop) pp_item

  let dump_cache =
    let pp_item f (id, workers) =
      Fmt.pf f "%s: %a"
        (id : Item.cache_hint :> string)
        Fmt.(Dump.list string) workers in
    Fmt.(hashtbl ~sep:comma) pp_item

  let dump_main f = function
    | `Backlog q -> Fmt.pf f "(backlog) %a" (dump_queue ~sep:Fmt.sp Item.pp) q
    | `Ready q -> Fmt.pf f "(ready) %a" (dump_queue pp_worker) q

  let dump f {pool = _; main; workers; cached; will_cache} =
    Fmt.pf f "@[<v>queue: @[%a@]@,@[<v2>registered:%a@]@,cached: @[%a@]@,will_cache: @[%a@]@]@."
      dump_main main
      dump_workers workers
      dump_cache cached
      dump_cache will_cache
end

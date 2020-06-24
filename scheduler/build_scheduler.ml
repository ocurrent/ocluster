open Astring
open Lwt.Infix
open Capnp_rpc_lwt

module Item = struct
  type t = {
    descr : Api.Queue.job_desc;
    set_job : Api.Raw.Service.Job.t Capability.resolver;
  }
end

module Worker_queue = struct
  type t = {
    name : string;
    mutable state : [ `Running of Item.t Queue.t * unit Lwt_condition.t | `Finished ];
  }

  let create name =
    {
      name;
      state = `Running (Queue.create (), Lwt_condition.create ());
    }

  let pop t ~name ~job =
    Log.info (fun f -> f "Worker %S ready" name);
    let rec aux () =
      match t.state with
      | `Finished -> Lwt_result.fail (`Capnp (Capnp_rpc.Error.exn "Worker disconnected"))
      | `Running (queue, cond) ->
        match Queue.take_opt queue with
        | None -> Lwt_condition.wait cond >>= aux
        | Some { set_job; descr } ->
          Capability.inc_ref job;
          Capability.resolve_ok set_job job;
          Lwt_result.return descr
    in
    aux ()
end

module Pool = struct
  type t = {
    incoming : Item.t Queue.t;
    queues : (string, Worker_queue.t) Hashtbl.t;
  }

  let rec assign (t : t) =
    match Queue.peek_opt t.incoming with
    | None -> Log.info (fun f -> f "Incoming queue is empty")
    | Some item ->
      match Hashtbl.to_seq t.queues () with
      | Nil -> Log.info (fun f -> f "No free workers");
      | Cons ((name, worker), _) ->
        Log.info (fun f -> f "Assigning to worker %S" name);
        match worker.state with
        | `Finished -> failwith "Worker is not active!"
        | `Running (queue, cond) ->
          Queue.add item queue;
          ignore (Queue.pop t.incoming);
          Lwt_condition.broadcast cond ();
          assign t

  let release (t : t) (w : Worker_queue.t) () =
    match w.state with
    | `Finished -> failwith "Queue already closed!"
    | `Running (queue, cond) ->
      Log.info (fun f -> f "Worker %S disconnected (reassigning %d items)" w.name (Queue.length queue));
      Queue.transfer queue t.incoming;
      w.state <- `Finished;
      Hashtbl.remove t.queues w.name;
      Lwt_condition.broadcast cond ();
      assign t

  let register t ~name =
    Log.info (fun f -> f "Registered new worker %S" name);
    if Hashtbl.mem t.queues name then
      Fmt.failwith "Worker already registered!";
    let q = Worker_queue.create name in
    Hashtbl.add t.queues name q;
    assign t;
    Api.Queue.local ~pop:(Worker_queue.pop q ~name) ~release:(release t q)

  let create () =
    {
      incoming = Queue.create ();
      queues = Hashtbl.create 10;
    }

  let registration_service t =
    let register = register t in
    Api.Registration.local ~register

  let submit t (descr : Api.Queue.job_desc) : Api.Job.t =
    let job, set_job = Capability.promise () in
    Log.info (fun f -> f "Received new job request");
    let item = { Item.descr; set_job } in
    Queue.add item t.incoming;
    assign t;
    job
end

type t = {
  pools : Pool.t String.Map.t;
}

let registration_services t =
  String.Map.map Pool.registration_service t.pools |> String.Map.bindings

let pp_pool_name f (name, _) = Fmt.string f name

let submission_service t =
  let submit ~pool descr =
    match String.Map.find_opt pool t.pools with
    | None ->
      let msg = Fmt.strf "Pool ID %S not one of @[<h>{%a}@]" pool (String.Map.pp ~sep:Fmt.comma pp_pool_name) t.pools in
      Capability.broken (Capnp_rpc.Exception.v msg)
    | Some pool ->
      Pool.submit pool descr
  in
  Api.Submission.local ~submit

let create pools =
  let pools =
    List.fold_left
      (fun acc name -> String.Map.add name (Pool.create ()) acc)
      String.Map.empty pools
  in
  { pools }

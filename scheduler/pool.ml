open Lwt.Infix

module Make (Item : sig type t end) = struct
  type t = {
    incoming : Item.t Queue.t;
    workers : (string, worker) Hashtbl.t;
  } and worker = {
    name : string;
    mutable state : [ `Running of Item.t Queue.t * unit Lwt_condition.t | `Finished ];
  }

  let pop worker =
    let rec aux () =
      match worker.state with
      | `Finished -> Lwt_result.fail `Finished
      | `Running (queue, cond) ->
        match Queue.take_opt queue with
        | None -> Lwt_condition.wait cond >>= aux
        | Some item -> Lwt_result.return item
    in
    aux ()

  let rec assign t =
    match Queue.peek_opt t.incoming with
    | None -> Log.info (fun f -> f "Incoming queue is empty")
    | Some item ->
      match Hashtbl.to_seq t.workers () with
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

  let release t w =
    match w.state with
    | `Finished -> failwith "Queue already closed!"
    | `Running (queue, cond) ->
      Log.info (fun f -> f "Worker %S disconnected (reassigning %d items)" w.name (Queue.length queue));
      Queue.transfer queue t.incoming;
      w.state <- `Finished;
      Hashtbl.remove t.workers w.name;
      Lwt_condition.broadcast cond ();
      assign t

  let register t ~name =
    if Hashtbl.mem t.workers name then Error `Name_taken
    else (
      let q = {
        name;
        state = `Running (Queue.create (), Lwt_condition.create ());
      } in
      Hashtbl.add t.workers name q;
      assign t;
      Ok q
    )

  let submit t item =
    Queue.add item t.incoming;
    assign t

  let create () =
    {
      incoming = Queue.create ();
      workers = Hashtbl.create 10;
    }
end

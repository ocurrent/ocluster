open Astring
open Lwt.Infix
open Capnp_rpc_lwt

type Sqlite_loader.Ty.t += Client
let () = Sqlite_loader.Ty.register "ocluster-client" Client

let restart_timeout = 600.0   (* Maximum time to wait for a worker to reconnect after it disconnects. *)

module Metrics = struct
  open Prometheus

  let namespace = "ocluster"
  let subsystem = "scheduler"

  let priority ~urgent =
    if urgent then "high" else "low"

  let client_queued_jobs =
    let help = "Items currently queued" in
    let f = Gauge.v_labels ~label_names:["client"; "pool"; "priority"] ~help ~namespace ~subsystem "client_submitted_jobs" in
    fun ~client_id ~pool ~urgent -> Gauge.labels f [client_id; pool; priority ~urgent]
end

module Item = struct
  type t = {
    descr : Cluster_api.Queue.job_desc;
    set_job : Cluster_api.Raw.Service.Job.t Capability.resolver;
  }

  type cache_hint = string

  let default_estimate = S.{
      cached = 10;                (* A build with cached dependencies usually only takes about 10 seconds. *)
      non_cached = 600;           (* If we have to install dependencies, it'll probably take about 10 minutes. *)
  }

  let cost_estimate _t = default_estimate

  let cache_hint t =
    Cluster_api.Raw.Reader.JobDescr.cache_hint_get t.descr

  let pp f t =
    match cache_hint t with
    | "" -> Fmt.string f "(no cache hint)"
    | x -> Fmt.string f x
end

module Pool_api = struct
  module Inactive_reasons = Pool.Inactive_reasons
  module Pool = Pool.Make(Item)(Unix)

  type t = {
    pool : Pool.t;
    workers : (string, Cluster_api.Worker.t) Hashtbl.t;
    cond : unit Lwt_condition.t;    (* Fires when a worker joins *)
  }

  let create ~name ~db =
    let pool = Pool.create ~name ~db in
    let workers = Hashtbl.create 10 in
    let cond = Lwt_condition.create () in
    { pool; workers; cond }

  let submit client ~urgent (descr : Cluster_api.Queue.job_desc) : Cluster_api.Ticket.t =
    let job, set_job = Capability.promise () in
    Log.info (fun f -> f "Received new job request from %S (urgent=%b)" (Pool.Client.client_id client) urgent);
    let item = { Item.descr; set_job } in
    let ticket = Pool.Client.submit ~urgent client item in
    let queued_jobs = Metrics.client_queued_jobs ~client_id:(Pool.Client.client_id client) ~pool:(Pool.Client.pool_id client) ~urgent in
    Prometheus.Gauge.inc_one queued_jobs;
    Lwt.async (fun () ->
        Capability.wait_until_settled job >|= fun () ->
        Prometheus.Gauge.dec_one queued_jobs
      );
    let cancel () =
      match Pool.Client.cancel client ticket with
      | Ok () ->
        Capability.resolve_exn set_job (Capnp_rpc.Exception.v "Ticket cancelled");
        Lwt_result.return ()
      | Error `Not_queued ->
        Cluster_api.Job.cancel job
    in
    let release () =
      match Pool.Client.cancel client ticket with
      | Ok () -> Capability.resolve_exn set_job (Capnp_rpc.Exception.v "Ticket released (cancelled)")
      | Error `Not_queued -> ()
    in
    Cluster_api.Ticket.local ~job ~cancel ~release

  let pop q ~job =
    Pool.pop q >|= function
    | Error `Finished -> Error (`Capnp (Capnp_rpc.Error.exn "Worker disconnected"))
    | Ok { set_job; descr } ->
      Capability.inc_ref job;
      Capability.resolve_ok set_job job;
      Ok descr

  let register t ~name ~capacity worker =
    match Pool.register t.pool ~name ~capacity with
    | Error `Name_taken as e ->
      Log.warn (fun f -> f "Worker %S already registered!" name);
      Capability.dec_ref worker;
      e
    | Ok q ->
      Pool.set_active q true ~reason:Inactive_reasons.worker;
      Log.info (fun f -> f "Registered new worker %S" name);
      Hashtbl.add t.workers name worker;
      Lwt_condition.broadcast t.cond ();
      let queue = Cluster_api.Queue.local
        ~pop:(pop q)
        ~set_active:(Pool.set_active ~reason:Inactive_reasons.worker q)
        ~release:(fun () ->
          Hashtbl.remove t.workers name;
          Capability.dec_ref worker;
          Pool.release q
        )
      in
      Ok queue

  let registration_service t =
    let register = register t in
    Cluster_api.Registration.local ~register

  let worker t name =
    match Hashtbl.find_opt t.workers name with
    | None -> None
    | Some w ->
      Capability.inc_ref w;
      Some w

  let admin_service ~validate_client t =
    let show () = Fmt.to_to_string Pool.show t.pool in
    let workers () = Pool.known_workers t.pool in
    let set_active ~auto_create name active =
      if auto_create || Pool.worker_known t.pool name then (
        Pool.with_worker t.pool name (fun worker ->
            Pool.set_active worker active ~reason:Inactive_reasons.admin_pause; Ok ()
          )
      ) else (
        Error `Unknown_worker
      )
    in
    let forget name =
      match Pool.forget_worker t.pool name with
      | Ok () -> Service.return_empty ()
      | Error `Still_connected -> Service.fail "Worker %S is still connected!" name
      | Error `Unknown_worker -> Service.fail "Worker %S not known in this pool" name
    in
    let update name =
      match Pool.Worker_map.find_opt name (Pool.connected_workers t.pool) with
      | None -> Service.fail "Unknown worker"
      | Some w ->
        let cap = Option.get (worker t name) in
        Pool.shutdown w;        (* Prevent any new items being assigned to it. *)
        Service.return_lwt @@ fun () ->
        Capability.with_ref cap @@ fun worker ->
        Log.info (fun f -> f "Restarting %S" name);
        Cluster_api.Worker.self_update worker >>= function
        | Error _ as e -> Lwt.return e
        | Ok () ->
          Log.info (fun f -> f "Waiting for %S to reconnect after update" name);
          let rec aux () =
            match Pool.Worker_map.find_opt name (Pool.connected_workers t.pool) with
            | Some new_w when new_w != w -> Lwt_result.return (Service.Response.create_empty ())
            | _ -> Lwt_condition.wait t.cond >>= aux
          in
          let timeout = 
            Lwt_unix.sleep restart_timeout >|= fun () ->
            Error (`Capnp (Capnp_rpc.Error.exn "Timeout waiting for worker to reconnect!"))
          in
          Lwt.pick [ aux (); timeout ]
    in
    let set_rate ~client_id rate =
      if validate_client client_id then (
        let client = Pool.client t.pool ~client_id in
        Pool.Client.set_rate client rate;
        Ok ()
      ) else Error `No_such_user
    in
    Cluster_api.Pool_admin.local ~show ~workers ~worker:(worker t) ~set_active ~update ~forget ~set_rate

  let remove_client t ~client_id =
    Pool.remove_client t.pool ~client_id
end

type t = {
  pools : Pool_api.t String.Map.t;
}

let registration_services t =
  String.Map.map Pool_api.registration_service t.pools |> String.Map.bindings

let pp_pool_name f (name, _) = Fmt.string f name

let submission_service ~validate ~sturdy_ref t client_id =
  let pools = Hashtbl.create 3 in
  let get_client pool_id =
    match Hashtbl.find_opt pools pool_id with
    | Some c -> Ok c
    | None ->
      match String.Map.find_opt pool_id t.pools with
      | None ->
        let msg = Fmt.strf "Pool ID %S not one of @[<h>{%a}@]" pool_id (String.Map.pp ~sep:Fmt.comma pp_pool_name) t.pools in
        Error (Capnp_rpc.Exception.v msg)
      | Some pool ->
        let client = Pool_api.Pool.client pool.pool ~client_id in
        Hashtbl.add pools pool_id client;
        Ok client
  in
  let submit ~pool ~urgent descr =
    match validate () with
    | false -> Capability.broken (Capnp_rpc.Exception.v "Access has been revoked")
    | true ->
      match get_client pool with
      | Ok client -> Pool_api.submit ~urgent client descr
      | Error ex -> Capability.broken ex
  in
  Cluster_api.Submission.local ~submit ~sturdy_ref

let pool t name =
  String.Map.find_opt name t.pools

let admin_service ~loader ~restore t =
  let pools () = String.Map.bindings t.pools |> List.map fst in
  let pool name =
    match String.Map.find_opt name t.pools with
    | None -> Capability.broken (Capnp_rpc.Exception.v "No such pool")
    | Some pool_api ->
      let validate_client id =
        match Sqlite_loader.lookup_by_descr loader (Client, id) with
        | [] -> false
        | _ -> true
      in
      Pool_api.admin_service ~validate_client pool_api
  in
  let add_client name =
    let descr = (Client, name) in
    match Sqlite_loader.lookup_by_descr loader descr with
    | [] ->
      let secret = Sqlite_loader.add loader descr in
      Log.info (fun f -> f "Created new submission endpoint for client %S" name);
      Capnp_rpc_net.Restorer.restore restore secret
      >|= Result.map_error (fun x -> `Capnp (`Exception x))
    | _ -> Lwt_result.fail (`Capnp (Capnp_rpc.Error.exn "Client %S already registered!" name))
  in
  let remove_client name =
    let descr = (Client, name) in
    match Sqlite_loader.lookup_by_descr loader descr with
    | [digest] ->
      Sqlite_loader.remove loader digest;
      t.pools |> String.Map.iter (fun _ -> Pool_api.remove_client ~client_id:name);
      Log.info (fun f -> f "Removed endpoint for client %S" name);
      Lwt_result.return ()
    | [] -> Lwt_result.fail (`Capnp (Capnp_rpc.Error.exn "Unknown client %S" name))
    | _ -> failwith "BUG: multiple users found with the same ID!"
  in
  let list_clients () =
    Sqlite_loader.list_by_type loader Client |> List.map (fun (_digest, name) -> name) |> Lwt.return
  in
  Cluster_api.Admin.local ~pools ~pool ~add_client ~remove_client ~list_clients

let create ~db pools =
  let db = Pool.Dao.init db in
  let pools =
    List.fold_left
      (fun acc name -> String.Map.add name (Pool_api.create ~name ~db) acc)
      String.Map.empty pools
  in
  { pools }

module S = S
module Pool = Pool
module Sqlite_loader = Sqlite_loader

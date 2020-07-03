open Astring
open Lwt.Infix
open Capnp_rpc_lwt

module Api = Build_scheduler_api

module Item = struct
  type t = {
    descr : Api.Queue.job_desc;
    set_job : Api.Raw.Service.Job.t Capability.resolver;
  }

  type cache_hint = string

  let default_estimate = S.{
      cached = 10;                (* A build with cached dependencies usually only takes about 10 seconds. *)
      non_cached = 600;           (* If we have to install dependencies, it'll probably take about 10 minutes. *)
  }

  let cost_estimate _t = default_estimate

  let cache_hint t =
    Api.Raw.Reader.JobDescr.cache_hint_get t.descr

  let pp f t =
    match cache_hint t with
    | "" -> Fmt.string f "(no cache hint)"
    | x -> Fmt.string f x
end

module Pool_api = struct
  module Pool = Pool.Make(Item)

  type t = {
    pool : Pool.t;
    workers : (string, Api.Worker.t) Hashtbl.t;
  }

  let create ~name ~db =
    let pool = Pool.create ~name ~db in
    let workers = Hashtbl.create 10 in
    { pool; workers }

  let submit t ~urgent (descr : Api.Queue.job_desc) : Api.Job.t =
    let job, set_job = Capability.promise () in
    Log.info (fun f -> f "Received new job request (urgent=%b)" urgent);
    let item = { Item.descr; set_job } in
    Pool.submit ~urgent t.pool item;
    job

  let pop q ~job =
    Pool.pop q >|= function
    | Error `Finished -> Error (`Capnp (Capnp_rpc.Error.exn "Worker disconnected"))
    | Ok { set_job; descr } ->
      Capability.inc_ref job;
      Capability.resolve_ok set_job job;
      Ok descr

  let register t ~name worker =
    match Pool.register t.pool ~name with
    | Error `Name_taken ->
      Fmt.failwith "Worker already registered!";
    | Ok q ->
      Log.info (fun f -> f "Registered new worker %S" name);
      Hashtbl.add t.workers name worker;
      Api.Queue.local ~pop:(pop q) ~release:(fun () ->
          Hashtbl.remove t.workers name;
          Capability.dec_ref worker;
          Pool.release q
        )

  let registration_service t =
    let register = register t in
    Api.Registration.local ~register

  let worker t name =
    match Hashtbl.find_opt t.workers name with
    | None -> None
    | Some w ->
      Capability.inc_ref w;
      Some w
end

type t = {
  pools : Pool_api.t String.Map.t;
}

let registration_services t =
  String.Map.map Pool_api.registration_service t.pools |> String.Map.bindings

let pp_pool_name f (name, _) = Fmt.string f name

let submission_service t =
  let submit ~pool ~urgent descr =
    match String.Map.find_opt pool t.pools with
    | None ->
      let msg = Fmt.strf "Pool ID %S not one of @[<h>{%a}@]" pool (String.Map.pp ~sep:Fmt.comma pp_pool_name) t.pools in
      Capability.broken (Capnp_rpc.Exception.v msg)
    | Some pool ->
      Pool_api.submit ~urgent pool descr
  in
  Api.Submission.local ~submit

let pool t name =
  String.Map.find_opt name t.pools

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

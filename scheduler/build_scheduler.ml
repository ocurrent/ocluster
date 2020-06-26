open Astring
open Lwt.Infix
open Capnp_rpc_lwt

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

  let pp f t = Fmt.string f (cache_hint t)
end

module Pool_api = struct
  module Pool = Pool.Make(Item)

  type t = Pool.t

  let create = Pool.create

  let submit t (descr : Api.Queue.job_desc) : Api.Job.t =
    let job, set_job = Capability.promise () in
    Log.info (fun f -> f "Received new job request");
    let item = { Item.descr; set_job } in
    Pool.submit t item;
    job

  let pop q ~job =
    Pool.pop q >|= function
    | Error `Finished -> Error (`Capnp (Capnp_rpc.Error.exn "Worker disconnected"))
    | Ok { set_job; descr } ->
      Capability.inc_ref job;
      Capability.resolve_ok set_job job;
      Ok descr

  let register t ~name =
    match Pool.register t ~name with
    | Error `Name_taken ->
      Fmt.failwith "Worker already registered!";
    | Ok q ->
      Log.info (fun f -> f "Registered new worker %S" name);
      Api.Queue.local ~pop:(pop q) ~release:(fun () -> Pool.release q)

  let registration_service t =
    let register = register t in
    Api.Registration.local ~register
end

type t = {
  pools : Pool_api.t String.Map.t;
}

let registration_services t =
  String.Map.map Pool_api.registration_service t.pools |> String.Map.bindings

let pp_pool_name f (name, _) = Fmt.string f name

let submission_service t =
  let submit ~pool descr =
    match String.Map.find_opt pool t.pools with
    | None ->
      let msg = Fmt.strf "Pool ID %S not one of @[<h>{%a}@]" pool (String.Map.pp ~sep:Fmt.comma pp_pool_name) t.pools in
      Capability.broken (Capnp_rpc.Exception.v msg)
    | Some pool ->
      Pool_api.submit pool descr
  in
  Api.Submission.local ~submit

let create pools =
  let pools =
    List.fold_left
      (fun acc name -> String.Map.add name (Pool_api.create ~name) acc)
      String.Map.empty pools
  in
  { pools }

module S = S
module Pool = Pool

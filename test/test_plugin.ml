open Capnp_rpc_lwt
open Lwt.Infix
open Current.Syntax

module Restorer = Capnp_rpc_net.Restorer

let make_sturdy _ = Uri.of_string "mock:sturdy"

let with_sched fn =
  let db = Sqlite3.db_open ":memory:" in
  Lwt.finalize
    (fun () ->
       let sched = Cluster_scheduler.create ~db ["pool"] in
       let load ~validate ~sturdy_ref = function
         | Cluster_scheduler.Client, name ->
           Lwt.return @@ Restorer.grant (Cluster_scheduler.submission_service ~validate ~sturdy_ref sched name)
         | (ty, _) -> Fmt.failwith "Unknown SturdyRef type %a found in database!" Cluster_scheduler.Sqlite_loader.Ty.pp ty
       in
       let loader = Cluster_scheduler.Sqlite_loader.create ~make_sturdy ~load db in
       let services = Restorer.Table.of_loader (module Cluster_scheduler.Sqlite_loader) loader in
       let restore = Restorer.of_table services in
       let pools = Cluster_scheduler.registration_services sched in
       match pools with
       | ["pool", registration_service] ->
         Capability.with_ref registration_service @@ fun registry ->
         Capability.with_ref (Cluster_scheduler.admin_service sched ~loader ~restore) @@ fun admin ->
         Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
         fn ~submission_service ~registry
       | _ -> failwith "Missing pool!"
    )
    (fun () -> if Sqlite3.db_close db then Lwt.return_unit else failwith "close: DB busy!")

module SVar = Current.Var(struct
    type t = (unit -> unit Current.t)
    let equal = (==)
    let pp f _ = Fmt.string f "pipeline"
  end)
let selected = SVar.create ~name:"current-test" (Error (`Msg "no-test"))

let test_pipeline =
  Current.component "choose pipeline" |>
  let** make_pipeline = SVar.get selected in
  make_pipeline ()

let engine_cond = Lwt_condition.create ()       (* Fires after each update *)

let setup ~pipeline fn =
  with_sched @@ fun ~submission_service ~registry ->
  let submission_service, break = Mock_network.remote_breakable submission_service in
  let t = Current_ocluster.v (Current_ocluster.Connection.create submission_service) in
  let state = ref (Error (`Msg "(init)")) in
  SVar.set selected (Ok (fun () -> pipeline t));
  let trace ~next:_ (results : Current.Engine.results) =
    state := results.value;
    Lwt_condition.broadcast engine_cond ();
    if results.value = Ok () then raise Exit;
    Lwt.return_unit
  in
  let engine = Current.Engine.create ~trace (fun () -> test_pipeline) in
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> Current.Engine.thread engine)
        (function
          | Exit -> Lwt.return_unit
          | ex ->
            Logs.err (fun f -> f "Engine exception: %a" Fmt.exn ex);
            raise ex
        )
    );
  let rec await_result () =
    match !state with
    | Error (`Active _) -> Lwt_condition.wait engine_cond >>= await_result
    | Ok _ | Error (`Msg _) as x -> Lwt.return x
  in
  fn ~registry ~await_result ~break >>= fun () ->
  Lwt.pause () >>= fun () ->
  Lwt.return_unit

let pipeline_result = Alcotest.of_pp @@ Current_term.Output.pp (fun f () -> Fmt.string f "()")

let options = Cluster_api.Docker.Spec.defaults

let simple () =
  let spec = `Contents (Current.return "example1") in
  let pipeline t = Current_ocluster.build t spec ~pool:"pool" ~src:(Current.return []) ~options in
  setup ~pipeline @@ fun ~registry ~await_result ~break:_ ->
  let builder = Mock_builder.create () in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run_remote builder ~network_switch:switch ~builder_switch:switch registry;
  Mock_builder.await builder "example1" >>= fun _ ->
  Mock_builder.set builder "example1" @@ Ok "hash";
  await_result () >>= fun x ->
  Alcotest.(check (result pass reject)) "Pipeline successful" (Ok ()) x;
  Lwt.return_unit

let disconnect_while_queued () =
  let spec = `Contents (Current.return "example2") in
  let pipeline t = Current_ocluster.build t spec ~pool:"pool" ~src:(Current.return []) ~options in
  setup ~pipeline @@ fun ~registry ~await_result ~break ->
  Lwt.pause () >>= fun () ->
  Lwt.pause () >>= fun () ->
  Lwt.pause () >>= fun () ->
  break () >>= fun () ->
  (* The plugin will immediately reconnect to the scheduler.
     Now add a worker and the job should run. *)
  let builder = Mock_builder.create () in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run_remote builder ~network_switch:switch ~builder_switch:switch registry;
  Mock_builder.set builder "example2" @@ Ok "hash";
  await_result () >>= fun x ->
  Alcotest.(check pipeline_result) "Retry succeeded" (Ok ()) x;
  Lwt.return_unit

let two_jobs () =
  let spec1 = `Contents (Current.return "spec1") in
  let spec2 = `Contents (Current.return "spec2") in
  let pipeline t =
    let open Current.Syntax in
    let* b1 = Current.state (Current_ocluster.build t spec1 ~pool:"pool" ~src:(Current.return []) ~options)
    and* b2 = Current.state (Current_ocluster.build t spec2 ~pool:"pool" ~src:(Current.return []) ~options) in
    Logs.info (fun f -> f "@[<v>b1 = %a@,b2 = %a@]"
                  (Current_term.Output.pp (Fmt.any "()")) b1
                  (Current_term.Output.pp (Fmt.any "()")) b2
              );
    match b1, b2 with
    | Ok (), Error (`Msg _)
    | Error (`Msg _), Ok () -> Current.return ()
    | Error (`Msg x), Error (`Msg y) -> Current.fail (Fmt.str "%s,%s" x y)
    | _ -> Current.active `Running
  in
  setup ~pipeline @@ fun ~registry ~await_result ~break:_ ->
  Gc.full_major ();
  let builder = Mock_builder.create () in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run_remote builder ~network_switch:switch ~builder_switch:switch registry;
  Lwt.choose [Mock_builder.await builder "spec1";
              Mock_builder.await builder "spec2"]
  >>= fun _ ->
  let jobs = Current.Job.jobs () |> Current.Job.Map.bindings in
  let is_queued j = Lwt.is_sleeping (Current.Job.start_time j) in
  let rec wait () =
    match List.find_opt (fun (_, j) -> not (is_queued j)) jobs with
    | Some (_, j) -> Lwt.return j
    | None -> Lwt_condition.wait engine_cond >>= wait
  in
  wait () >>= fun started_job ->
  Lwt.pause () >>= fun () ->
  Gc.full_major ();
  Current.Job.cancel started_job "Cancelled by user";
  Mock_builder.set builder "spec1" @@ Ok "hash1";
  Mock_builder.set builder "spec2" @@ Ok "hash2";
  await_result () >>= fun x ->
  Alcotest.(check pipeline_result) "Pipeline successful" (Ok ()) x;
  Lwt.return_unit

let cancel_rate_limit () =
  let sched, set_sched = Lwt.wait () in
  let sched_sr =
    Capnp_rpc_lwt.Cast.sturdy_of_raw @@ object
      method connect = sched
      method to_uri_with_secrets = failwith "mock to_uri_with_secrets"
    end
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:0 sched_sr in
  (* Test cancelling while connecting to scheduler. *)
  let submit () =
    let action = Cluster_api.Submission.docker_build (`Path "Dockerfile") in
    let switch = Current.Switch.create ~label:"job" () in
    let config = Current.Config.v () in
    let job = Current.Job.create ~switch ~label:"job" ~config () in
    let pool = Current_ocluster.Connection.pool ~job ~pool:"test" ~action ~cache_hint:"" conn in
    job, Current.Job.use_pool ~switch job pool
  in
  let job, resource = submit () in
  (* We are now trying to connect to the scheduler. Cancel the job. *)
  Current.Job.cancel job "Test cancelling";
  Lwt.try_bind
    (fun () -> resource)
    (fun _ -> failwith "Should have failed!")
    (function
      | Lwt.Canceled -> Lwt.return_unit
      | ex -> Lwt.fail ex)
  >>= fun () ->
  (* Finish connecting to the scheduler. *)
  let sched =
    let submit ~pool:_ ~urgent:_ _job = assert false in
    let sturdy_ref = Capnp_rpc_lwt.Cast.sturdy_of_raw @@ object
        method connect = assert false
        method to_uri_with_secrets = assert false
      end
    in
    Cluster_api.Submission.local ~submit ~sturdy_ref
  in
  Lwt.wakeup set_sched (Ok (Cast.cap_to_raw sched));
  (* Submit another job. *)
  let job, resource = submit () in
  Current.Job.cancel job "Test cancelling";
  Lwt.try_bind
    (fun () -> resource)
    (fun _ -> failwith "Should have failed!")
    (function
      | Lwt.Canceled -> Lwt.return_unit
      | ex -> Lwt.fail ex)
  >>= fun () ->
  Lwt.return_unit

let test_case name fn =
  Alcotest_lwt.test_case name `Quick @@ fun _ () ->
  fn () >|= fun () ->
  Prometheus.CollectorRegistry.(collect default)
  |> Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output
  |> String.split_on_char '\n'
  |> List.iter (fun line ->
      if Astring.String.is_prefix ~affix:"scheduler_pool_" line ||
         Astring.String.is_prefix ~affix:"ocluster_ocurrent_" line
      then (
        match Astring.String.cut ~sep:"} " line with
        | None -> Fmt.failwith "Bad metrics line: %S" line
        | Some (key, _) when Astring.String.is_infix ~affix:"_total{" key -> ()
        | Some (key, value) ->
          if float_of_string value <> 0.0 then
            Fmt.failwith "Non-zero metric after test: %s}=%s" key value
      )
    )

let suite = [
  test_case "simple" simple;
  test_case "disconnect_while_queued" disconnect_while_queued;
  test_case "two_jobs" two_jobs;
  test_case "cancel_rate_limit" cancel_rate_limit;
]

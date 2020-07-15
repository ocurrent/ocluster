open Capnp_rpc_lwt
open Lwt.Infix
open Current.Syntax

let with_sched fn =
  let db = Sqlite3.db_open ":memory:" in
  Lwt.finalize
    (fun () ->
       let sched = Cluster_scheduler.create ~db ["pool"] in
       let pools = Cluster_scheduler.registration_services sched in
       match pools with
       | ["pool", registration_service] ->
         Capability.with_ref registration_service @@ fun registry ->
         Capability.with_ref (Cluster_scheduler.submission_service sched) @@ fun submission_service ->
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

let setup ~pipeline fn =
  with_sched @@ fun ~submission_service ~registry ->
  let submission_service, break = Mock_network.remote_breakable submission_service in
  let t = Current_ocluster.v submission_service in
  let state = ref (Error (`Msg "(init)")) in
  let cond = Lwt_condition.create () in
  SVar.set selected (Ok (fun () -> pipeline t));
  let trace ~next:_ (results : Current.Engine.results) =
    state := results.value;
    Lwt_condition.broadcast cond ();
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
    | Error (`Active _) -> Lwt_condition.wait cond >>= await_result
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
                  (Current_term.Output.pp (Fmt.unit "()")) b1
                  (Current_term.Output.pp (Fmt.unit "()")) b2
              );
    match b1, b2 with
    | Ok (), Error _
    | Error _, Ok () -> Current.return ()
    | _ -> Current.active `Running
  in
  setup ~pipeline @@ fun ~registry ~await_result ~break:_ ->
  let builder = Mock_builder.create () in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run_remote builder ~network_switch:switch ~builder_switch:switch registry;
  Lwt.choose [Mock_builder.await builder "spec1";
              Mock_builder.await builder "spec2"]
  >>= fun _ ->
  let jobs = Current.Job.jobs () |> Current.Job.Map.bindings in
  let _, started_job = List.find (fun (_, j) -> not (Lwt.is_sleeping (Current.Job.start_time j))) jobs in
  Current.Job.cancel started_job "Cancelled by user";
  Mock_builder.set builder "spec1" @@ Ok "hash1";
  Mock_builder.set builder "spec2" @@ Ok "hash2";
  await_result () >>= fun x ->
  Alcotest.(check (result pass reject)) "Pipeline successful" (Ok ()) x;
  Lwt.return_unit

let test_case name fn =
  Alcotest_lwt.test_case name `Quick @@ fun _ () ->
  fn () >|= fun () ->
  Prometheus.CollectorRegistry.(collect default)
  |> Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output
  |> String.split_on_char '\n'
  |> List.iter (fun line ->
      if Astring.String.is_prefix ~affix:"scheduler_pool_" line then (
        match Astring.String.cut ~sep:"} " line with
        | None -> Fmt.failwith "Bad metrics line: %S" line
        | Some (key, value) ->
          if float_of_string value <> 0.0 then
            Fmt.failwith "Non-zero metric after test: %s}=%s" key value
      )
    )

let suite = [
  test_case "simple" simple;
  test_case "disconnect_while_queued" disconnect_while_queued;
  test_case "two_jobs" two_jobs;
]

open Capnp_rpc_lwt
open Lwt.Infix

(* Setting this to true shows log output, which is useful if the tests hang.
   However, it also hides the Alcotest diff if it fails. *)
let verbose = false
let log_level = Logs.Info

let () =
  Printexc.record_backtrace true

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string) (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some log_level));
  (* Logs.Src.set_level Capnp_rpc.Debug.src (Some Logs.Debug); *)
  Logs.set_reporter reporter

let read_log job =
  let buffer = Buffer.create 1024 in
  let rec aux start =
    Cluster_api.Job.log job start >>= function
    | Error (`Capnp e) -> Lwt.return (Fmt.strf "Error tailing logs: %a@." Capnp_rpc.Error.pp e)
    | Ok ("", _) -> Lwt.return (Buffer.contents buffer)
    | Ok (data, next) ->
      Buffer.add_string buffer data;
      aux next
  in
  aux 0L

let submit service dockerfile =
  let action = Cluster_api.Submission.docker_build (`Contents dockerfile) in
  Capability.with_ref (Cluster_api.Submission.submit service ~pool:"pool" ~action ~cache_hint:"1" ?src:None) @@ fun ticket ->
  Capability.with_ref (Cluster_api.Ticket.job ticket) @@ fun job ->
  let result = Cluster_api.Job.result job in
  read_log job >>= fun log ->
  result >|= function
  | Ok "" -> log
  | Ok x -> Fmt.failwith "Unexpected job output: %S" x
  | Error (`Capnp _) -> Fmt.strf "%sFAILED@." log

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

(* Build on a single worker. *)
let simple () =
  with_sched @@ fun ~submission_service ~registry ->
  let builder = Mock_builder.create () in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  let result = submit submission_service "example" in
  Mock_builder.set builder "example" @@ Ok "hash";
  result >>= fun result ->
  Logs.app (fun f -> f "Result: %S" result);
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example\nJob succeeded\n" result;
  Lwt.return_unit

(* A failing build on a single worker. *)
let fails () =
  let builder = Mock_builder.create () in
  with_sched @@ fun ~submission_service ~registry ->
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  let result = submit submission_service "example2" in
  Mock_builder.set builder "example2" @@ Error (`Msg "Build failed!");
  result >>= fun result ->
  Logs.app (fun f -> f "Result: %S" result);
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example2\nBuild failed!\nFAILED\n" result;
  Lwt.return_unit

(* The job is submitted before any builders are registered. *)
let await_builder () =
  let builder = Mock_builder.create () in
  with_sched @@ fun ~submission_service ~registry ->
  let result = submit submission_service "example" in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  Mock_builder.set builder "example" @@ Ok "hash";
  result >>= fun result ->
  Logs.app (fun f -> f "Result: %S" result);
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example\nJob succeeded\n" result;
  Lwt.return_unit

(* A single builder can't do all the jobs and they queue up. *)
let builder_capacity () =
  let builder = Mock_builder.create () in
  with_sched @@ fun ~submission_service ~registry ->
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry) ~capacity:2;
  let r1 = submit submission_service "example1" in
  let r2 = submit submission_service "example2" in
  let r3 = submit submission_service "example3" in
  Lwt.pause () >>= fun () ->
  Mock_builder.set builder "example1" @@ Ok "hash";
  Mock_builder.set builder "example2" @@ Ok "hash";
  Mock_builder.set builder "example3" @@ Ok "hash";
  r1 >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example1\nJob succeeded\n" result;
  r2 >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example2\nJob succeeded\n" result;
  r3 >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example3\nJob succeeded\n" result;
  Lwt.return_unit

let admin () =
  let db = Sqlite3.db_open ":memory:" in
  let sched = Cluster_scheduler.create ~db ["pool"] in
  Capability.with_ref (Cluster_scheduler.admin_service sched) @@ fun admin ->
  Cluster_api.Admin.pools admin >>= fun pools ->
  Alcotest.(check (list string)) "Check pools" ["pool"] pools;
  Capability.with_ref (Cluster_api.Admin.pool admin "pool") @@ fun pool ->
  Cluster_api.Pool_admin.show pool >>= fun state ->
  Logs.info (fun f -> f "Pool state:\n%s" state);
  Lwt.return_unit

(* Test our mock network. *)
let network () =
  Lwt_switch.with_switch (fun network_switch ->
      let builder = Mock_builder.create () in
      with_sched @@ fun ~submission_service ~registry ->
      Lwt_switch.with_switch @@ fun builder_switch ->
      Mock_builder.run_remote builder ~network_switch ~builder_switch registry;
      let result = submit submission_service "example" in
      Mock_builder.set builder "example" @@ Ok "hash";
      result >>= fun result ->
      Logs.app (fun f -> f "Result: %S" result);
      Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example\nJob succeeded\n" result;
      Lwt.return_unit
    ) >>= fun () ->
  Lwt.pause ()

(* The worker disconnects. *)
let worker_disconnects () =
  let network_switch = Lwt_switch.create () in
  Lwt_switch.with_switch @@ fun builder_switch ->
  let builder = Mock_builder.create () in
  with_sched @@ fun ~submission_service ~registry ->
  Mock_builder.run_remote builder ~builder_switch ~network_switch registry;
  (* Run a job to ensure it's connected. *)
  let result = submit submission_service "example" in
  Mock_builder.set builder "example" @@ Ok "hash";
  result >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example\nJob succeeded\n" result;
  (* Drop network *)
  Logs.info (fun f -> f "Dropping worker's network connection");
  Lwt_switch.turn_off network_switch >>= fun () ->
  Lwt.pause () >>= fun () ->
  (* Try again *)
  let result = submit submission_service "example" in
  (* Worker reconnects *)
  let network_switch = Lwt_switch.create () in
  Mock_builder.run_remote builder ~builder_switch ~network_switch registry;
  Mock_builder.set builder "example" @@ Ok "hash";
  result >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example\nJob succeeded\n" result;
  Lwt.return_unit

(* The client gets disconnected. The job is automatically cancelled. *)
let client_disconnects () =
  let builder = Mock_builder.create () in
  with_sched @@ fun ~submission_service ~registry ->
  Lwt_switch.with_switch @@ fun builder_switch ->
  let net_switch = Lwt_switch.create () in
  Sturdy_ref.connect_exn (Mock_network.remote ~switch:net_switch submission_service) >>= fun submission_service ->
  Mock_builder.run builder (Mock_network.sturdy registry) ~switch:builder_switch;
  (* Start a job: *)
  let result = submit submission_service "example" in
  Mock_builder.await builder "example" >>= fun job_result ->
  (* Drop network *)
  Logs.info (fun f -> f "Drop network");
  Lwt_switch.turn_off net_switch >>= fun () ->
  result >>= fun result ->
  Alcotest.(check string) "Job failed!" "Error tailing logs: Disconnected: Connection closed\nFAILED\n" result;
  (* Check job is cancelled. *)
  Logs.info (fun f -> f "Wait for job to stop");
  job_result >|= function
  | Error `Cancelled -> ()
  | _ -> Alcotest.fail "Job should have been cancelled!"

(* The client cancels the job explicitly. *)
let cancel () =
  with_sched @@ fun ~submission_service ~registry ->
  let builder = Mock_builder.create () in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  let action = Cluster_api.Submission.docker_build (`Contents "example") in
  Capability.with_ref (Cluster_api.Submission.submit submission_service ~pool:"pool" ~action ~cache_hint:"1" ?src:None) @@ fun ticket ->
  Capability.with_ref (Cluster_api.Ticket.job ticket) @@ fun job ->
  let log = read_log job in
  Mock_builder.await builder "example" >>= fun _ ->
  Cluster_api.Job.cancel job >>= fun cancel_result ->
  Alcotest.(check (result unit reject)) "Cancel succeeds" (Ok ()) cancel_result;
  log >>= fun log ->
  Alcotest.(check string) "Check log" "Building on worker-1\nBuilding example\nJob cancelled\n" log;
  Cluster_api.Job.result job >>= fun result ->
  let result = Result.map_error (fun (`Capnp e) -> Fmt.to_to_string Capnp_rpc.Error.pp e) result in
  Alcotest.(check (result reject string)) "Check job failed" (Error "Failed: Build cancelled") result;
  Lwt.return_unit

(* The client cancels the ticket. *)
let cancel_ticket () =
  with_sched @@ fun ~submission_service ~registry:_ ->
  let action = Cluster_api.Submission.docker_build (`Contents "example") in
  Capability.with_ref (Cluster_api.Submission.submit submission_service ~pool:"pool" ~action ~cache_hint:"1" ?src:None) @@ fun ticket ->
  Capability.with_ref (Cluster_api.Ticket.job ticket) @@ fun job ->
  let result = Cluster_api.Job.result job in
  let log = read_log job in
  Cluster_api.Ticket.cancel ticket >>= fun cancel_result ->
  Alcotest.(check (result unit reject)) "Cancel succeeds" (Ok ()) cancel_result;
  log >>= fun log ->
  Alcotest.(check string) "Check log" "Error tailing logs: Failed: Ticket cancelled\n" log;
  result >>= fun result ->
  let result = Result.map_error (fun (`Capnp e) -> Fmt.to_to_string Capnp_rpc.Error.pp e) result in
  Alcotest.(check (result reject string)) "Check job failed" (Error "Failed: Ticket cancelled") result;
  Lwt.return_unit

(* The client cancels the ticket after the job is assigned. *)
let cancel_ticket_late () =
  with_sched @@ fun ~submission_service ~registry ->
  let builder = Mock_builder.create () in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  let action = Cluster_api.Submission.docker_build (`Contents "example") in
  Capability.with_ref (Cluster_api.Submission.submit submission_service ~pool:"pool" ~action ~cache_hint:"1" ?src:None) @@ fun ticket ->
  Capability.with_ref (Cluster_api.Ticket.job ticket) @@ fun job ->
  let result = Cluster_api.Job.result job in
  let log = read_log job in
  Mock_builder.await builder "example" >>= fun _ ->
  Cluster_api.Ticket.cancel ticket >>= fun cancel_result ->
  Alcotest.(check (result unit reject)) "Cancel succeeds" (Ok ()) cancel_result;
  log >>= fun log ->
  Alcotest.(check string) "Check log" "Building on worker-1\nBuilding example\nJob cancelled\n" log;
  result >>= fun result ->
  let result = Result.map_error (fun (`Capnp e) -> Fmt.to_to_string Capnp_rpc.Error.pp e) result in
  Alcotest.(check (result reject string)) "Check job failed" (Error "Failed: Build cancelled") result;
  Lwt.return_unit

(* The client releases the ticket before the job is assigned. *)
let release_ticket () =
  with_sched @@ fun ~submission_service ~registry:_ ->
  let action = Cluster_api.Submission.docker_build (`Contents "example") in
  let ticket = Cluster_api.Submission.submit submission_service ~pool:"pool" ~action ~cache_hint:"1" ?src:None in
  let job = Cluster_api.Ticket.job ticket in
  let result = Cluster_api.Job.result job in
  let log = read_log job in
  Capability.dec_ref job;
  Capability.dec_ref ticket;
  log >>= fun log ->
  Alcotest.(check string) "Check log" "Error tailing logs: Failed: Ticket released (cancelled)\n" log;
  result >>= fun result ->
  let result = Result.map_error (fun (`Capnp e) -> Fmt.to_to_string Capnp_rpc.Error.pp e) result in
  Alcotest.(check (result string string)) "Check ticket cancelled" (Error "Failed: Ticket released (cancelled)") result;
  Lwt.return_unit

(* The client releases the ticket after the job is assigned. *)
let release_ticket_late () =
  with_sched @@ fun ~submission_service ~registry ->
  let builder = Mock_builder.create () in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  let action = Cluster_api.Submission.docker_build (`Contents "example") in
  let ticket = Cluster_api.Submission.submit submission_service ~pool:"pool" ~action ~cache_hint:"1" ?src:None in
  let job = Cluster_api.Ticket.job ticket in
  let result = Cluster_api.Job.result job in
  let log = read_log job in
  Mock_builder.await builder "example" >>= fun _ ->
  Capability.dec_ref job;
  Capability.dec_ref ticket;
  Mock_builder.set builder "example" @@ Ok "hash";
  log >>= fun log ->
  Alcotest.(check string) "Check log" "Building on worker-1\nBuilding example\nJob cancelled\n" log;
  result >>= fun result ->
  let result = Result.map_error (fun (`Capnp e) -> Fmt.to_to_string Capnp_rpc.Error.pp e) result in
  Alcotest.(check (result string string)) "Check job cancelled" (Error "Failed: Build cancelled") result;
  Lwt.return_unit

let test_case name fn =
  Alcotest_lwt.test_case name `Quick @@ fun _ () ->
  let problems = Logs.(warn_count () + err_count ()) in
  fn () >>= fun () ->
  Lwt.pause () >>= fun () ->
  Gc.full_major ();
  Lwt.pause () >|= fun () ->
  let problems' = Logs.(warn_count () + err_count ()) in
  Alcotest.(check int) "Check log for warnings" 0 (problems' - problems)

let () =
  Lwt_main.run @@ Alcotest_lwt.run ~verbose "build-scheduler" [
    "main", [
      test_case "simple" simple;
      test_case "fails" fails;
      test_case "await_builder" await_builder;
      test_case "builder_capacity" builder_capacity;
      test_case "network" network;
      test_case "worker_disconnects" worker_disconnects;
      test_case "client_disconnects" client_disconnects;
      test_case "cancel" cancel;
      test_case "cancel_ticket" cancel_ticket;
      test_case "cancel_ticket_late" cancel_ticket_late;
      test_case "release_ticket" release_ticket;
      test_case "release_ticket_late" release_ticket_late;
      test_case "admin" admin;
    ];
    "scheduling", Test_scheduling.suite;
    "plugin", Test_plugin.suite;
  ]

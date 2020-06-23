open Capnp_rpc_lwt
open Lwt.Infix

(* Setting this to true shows log output, which is useful if the tests hang.
   However, it also hides the Alcotest diff if it fails. *)
let verbose = false
let log_level = Logs.Info

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
  Logs.set_reporter reporter

let read_log job =
  let buffer = Buffer.create 1024 in
  let rec aux start =
    Api.Job.log job start >>= function
    | Error (`Capnp e) -> Fmt.failwith "Error tailing logs: %a" Capnp_rpc.Error.pp e
    | Ok ("", _) -> Lwt.return (Buffer.contents buffer)
    | Ok (data, next) ->
      Buffer.add_string buffer data;
      aux next
  in
  aux 0L

let submit service dockerfile =
  Capability.with_ref (Api.Submission.submit service ~dockerfile ~cache_hint:"1" ?src:None) @@ fun job ->
  read_log job >>= fun log ->
  Api.Job.status job >|= function
  | Ok () -> log
  | Error (`Capnp _) -> Fmt.strf "%sFAILED@." log

(* Build on a single worker. *)
let simple () =
  let builder = Mock_builder.create () in
  let sched = Build_scheduler.create () in
  Capability.with_ref (Build_scheduler.submission_service sched) @@ fun submission_service ->
  Capability.with_ref (Build_scheduler.registration_service sched) @@ fun registry ->
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  let result = submit submission_service "example" in
  Mock_builder.set builder "example" @@ Ok ();
  result >>= fun result ->
  Logs.app (fun f -> f "Result: %S" result);
  Alcotest.(check string) "Check job worked" "Building example\nJob succeeded\n" result;
  Lwt.return_unit

(* A failing build on a single worker. *)
let fails () =
  let builder = Mock_builder.create () in
  let sched = Build_scheduler.create () in
  Capability.with_ref (Build_scheduler.submission_service sched) @@ fun submission_service ->
  Capability.with_ref (Build_scheduler.registration_service sched) @@ fun registry ->
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  let result = submit submission_service "example2" in
  Mock_builder.set builder "example2" @@ Error (`Exit_code 1);
  result >>= fun result ->
  Logs.app (fun f -> f "Result: %S" result);
  Alcotest.(check string) "Check job worked" "Building example2\nDocker build exited with status 1\nFAILED\n" result;
  Lwt.return_unit

(* The job is submitted before any builders are registered. *)
let await_builder () =
  let builder = Mock_builder.create () in
  let sched = Build_scheduler.create () in
  Capability.with_ref (Build_scheduler.submission_service sched) @@ fun submission_service ->
  Capability.with_ref (Build_scheduler.registration_service sched) @@ fun registry ->
  let result = submit submission_service "example" in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  Mock_builder.set builder "example" @@ Ok ();
  result >>= fun result ->
  Logs.app (fun f -> f "Result: %S" result);
  Alcotest.(check string) "Check job worked" "Building example\nJob succeeded\n" result;
  Lwt.return_unit

(* A single builder can't do all the jobs and they queue up. *)
let builder_capacity () =
  let builder = Mock_builder.create () in
  let sched = Build_scheduler.create () in
  Capability.with_ref (Build_scheduler.submission_service sched) @@ fun submission_service ->
  Capability.with_ref (Build_scheduler.registration_service sched) @@ fun registry ->
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry) ~capacity:2;
  let r1 = submit submission_service "example1" in
  let r2 = submit submission_service "example2" in
  let r3 = submit submission_service "example3" in
  Lwt.pause () >>= fun () ->
  Mock_builder.set builder "example1" @@ Ok ();
  Mock_builder.set builder "example2" @@ Ok ();
  Mock_builder.set builder "example3" @@ Ok ();
  r1 >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building example1\nJob succeeded\n" result;
  r2 >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building example2\nJob succeeded\n" result;
  r3 >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building example3\nJob succeeded\n" result;
  Lwt.return_unit

(* Test our mock network. *)
let network () =
  Lwt_switch.with_switch (fun network_switch ->
      let builder = Mock_builder.create () in
      let sched = Build_scheduler.create () in
      Capability.with_ref (Build_scheduler.submission_service sched) @@ fun submission_service ->
      Capability.with_ref (Build_scheduler.registration_service sched) @@ fun registry ->
      Lwt_switch.with_switch @@ fun builder_switch ->
      Mock_builder.run_remote builder ~network_switch ~builder_switch registry;
      let result = submit submission_service "example" in
      Mock_builder.set builder "example" @@ Ok ();
      result >>= fun result ->
      Logs.app (fun f -> f "Result: %S" result);
      Alcotest.(check string) "Check job worked" "Building example\nJob succeeded\n" result;
      Lwt.return_unit
    ) >>= fun () ->
  Lwt.pause ()

(* The worker disconnects. *)
let worker_disconnects () =
  let network_switch = Lwt_switch.create () in
  Lwt_switch.with_switch @@ fun builder_switch ->
  let builder = Mock_builder.create () in
  let sched = Build_scheduler.create () in
  Capability.with_ref (Build_scheduler.submission_service sched) @@ fun submission_service ->
  Capability.with_ref (Build_scheduler.registration_service sched) @@ fun registry ->
  Mock_builder.run_remote builder ~builder_switch ~network_switch registry;
  (* Run a job to ensure it's connected. *)
  let result = submit submission_service "example" in
  Mock_builder.set builder "example" @@ Ok ();
  result >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building example\nJob succeeded\n" result;
  (* Drop network *)
  Logs.info (fun f -> f "Dropping worker's network connection");
  Lwt_switch.turn_off network_switch >>= fun () ->
  Lwt.pause () >>= fun () ->
  (* Try again *)
  let result = submit submission_service "example" in
  (* Worker reconnects *)
  let network_switch = Lwt_switch.create () in
  Mock_builder.run_remote builder ~builder_switch ~network_switch registry;
  Mock_builder.set builder "example" @@ Ok ();
  result >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building example\nJob succeeded\n" result;
  Lwt.return_unit

(* The client gets disconnected. The job is automatically cancelled. *)
let client_disconnects () =
  let builder = Mock_builder.create () in
  let sched = Build_scheduler.create () in
  Capability.with_ref (Build_scheduler.submission_service sched) @@ fun submission_service ->
  Lwt_switch.with_switch @@ fun builder_switch ->
  let net_switch = Lwt_switch.create () in
  Sturdy_ref.connect_exn (Mock_network.remote ~switch:net_switch submission_service) >>= fun submission_service ->
  Capability.with_ref (Build_scheduler.registration_service sched) @@ fun registry ->
  Mock_builder.run builder (Mock_network.sturdy registry) ~switch:builder_switch;
  (* Start a job: *)
  let result = submit submission_service "example" in
  Mock_builder.await builder "example" >>= fun job_result ->
  (* Drop network *)
  Lwt_switch.turn_off net_switch >>= fun () ->
  Lwt_result.catch result >>= function
  | Ok _ -> Alcotest.fail "Job should have failed!"
  | Error ex ->
    Logs.info (fun f -> f "Client got disconnected: %a" Fmt.exn ex);
    (* Check job is cancelled. *)
    Logs.info (fun f -> f "Wait for job to stop");
    job_result >|= function
    | Error `Cancelled -> ()
    | _ -> Alcotest.fail "Job should have been cancelled!"

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
    ]
  ]

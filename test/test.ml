open Capnp_rpc_lwt
open Lwt.Infix

module Restorer = Capnp_rpc_net.Restorer

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
    | Error (`Capnp e) -> Lwt.return (Fmt.str "Error tailing logs: %a@." Capnp_rpc.Error.pp e)
    | Ok ("", _) -> Lwt.return (Buffer.contents buffer)
    | Ok (data, next) ->
      Buffer.add_string buffer data;
      aux next
  in
  aux 0L

let common_submit service action =
  Capability.with_ref (Cluster_api.Submission.submit service ~pool:"pool" ~action ~cache_hint:"1" ?src:None) @@ fun ticket ->
  Capability.with_ref (Cluster_api.Ticket.job ticket) @@ fun job ->
  let result = Cluster_api.Job.result job in
  read_log job >>= fun log ->
  result >|= function
  | Ok "" -> log
  | Ok x -> Fmt.failwith "Unexpected job output: %S" x
  | Error (`Capnp _) -> Fmt.str "%sFAILED@." log

let submit service dockerfile =
  let action = Cluster_api.Submission.docker_build (`Contents dockerfile) in
  common_submit service action

let custom_submit service c =
  let action = Cluster_api.Submission.custom_build c in
  common_submit service action

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
       let make_sturdy _ = Uri.of_string "mock:sturdy" in
       let loader = Cluster_scheduler.Sqlite_loader.create ~make_sturdy ~load db in
       let services = Restorer.Table.of_loader (module Cluster_scheduler.Sqlite_loader) loader in
       let restore = Restorer.of_table services in
       let pools = Cluster_scheduler.registration_services sched in
       match pools with
       | ["pool", registration_service] ->
         Capability.with_ref registration_service @@ fun registry ->
         Capability.with_ref (Cluster_scheduler.admin_service sched ~restore ~loader) @@ fun admin ->
         fn ~admin ~registry
       | _ -> failwith "Missing pool!"
    )
    (fun () -> if Sqlite3.db_close db then Lwt.return_unit else failwith "close: DB busy!")

(* Build on a single worker. *)
let simple () =
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
  let builder = Mock_builder.create () in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  let result = submit submission_service "example" in
  Mock_builder.set builder "example" @@ Ok "hash";
  result >>= fun result ->
  Logs.app (fun f -> f "Result: %S" result);
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example\nJob succeeded\n" result;
  Lwt.return_unit

let simple_custom () =
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
  let builder = Mock_builder.create () in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  let kind = "obuilder" in
  let spec = "((from ocaml/opam:latest)\n(run (shell \"ls\")))" in
  let job = Cluster_api.Custom.v ~kind @@ Custom.Spec.obuilder_spec_to_custom spec in
  let result = custom_submit submission_service job in
  Mock_builder.set builder "obuilder" @@ Ok "";
  result >>= fun result ->
  Logs.app (fun f -> f "Result: %S" result);
  let expect = "Building on worker-1\nBuilding job obuilder\nGot spec " ^ spec ^  "\nJob succeeded\n" in
  Alcotest.(check string) "Check job worked" expect result;
  Lwt.return_unit

(* A failing build on a single worker. *)
let fails () =
  let builder = Mock_builder.create () in
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
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
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
  let result = submit submission_service "example" in
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry);
  Mock_builder.set builder "example" @@ Ok "hash";
  result >>= fun result ->
  Logs.app (fun f -> f "Result: %S" result);
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example\nJob succeeded\n" result;
  Lwt.return_unit

(* Two workers register with the same name. *)
let already_registered () =
  with_sched @@ fun ~admin:_ ~registry ->
  let api = Cluster_api.Worker.local ~metrics:(fun _ -> assert false) ~self_update:(fun () -> assert false) () in
  let q1 = Cluster_api.Registration.register registry ~name:"worker-1" ~capacity:1 api in
  Capability.await_settled q1 >>= fun (_ : _ result) ->
  let q2 = Cluster_api.Registration.register registry ~name:"worker-1" ~capacity:1 api in
  Capability.await_settled q2 >>= fun (_ : _ result) ->
  let pp_err = Fmt.(option ~none:(any "ok")) Capnp_rpc.Exception.pp in
  let p1 = Fmt.str "%a" pp_err (Capability.problem q1) in
  let p2 = Fmt.str "%a" pp_err (Capability.problem q2) in
  Alcotest.(check string) "First worker connected" "ok" p1;
  Alcotest.(check string) "Second was rejected" "Failed: Worker already registered!" p2;
  Capability.dec_ref q1;
  Capability.dec_ref api;
  Lwt.return_unit

(* A single builder can't do all the jobs and they queue up. *)
let builder_capacity () =
  let builder = Mock_builder.create () in
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
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

let expect_status_change status prev next =
  let rec aux () =
    if !status = prev then Lwt.pause () >>= aux
    else (
      Alcotest.(check string) "Status" next !status;
      Lwt.return_unit
    )
  in aux ()

(* The admin drains a worker. *)
let drain () =
  let builder = Mock_builder.create () in
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
  Lwt_switch.with_switch @@ fun switch ->
  Mock_builder.run ~switch builder (Mock_network.sturdy registry) ~capacity:2;
  let r1 = submit submission_service "example1" in
  let r2 = submit submission_service "example2" in
  let r3 = submit submission_service "example3" in
  Lwt.pause () >>= fun () ->
  let pool = Cluster_api.Admin.pool admin "pool" in
  let status = ref "" in
  let progress = Cluster_api.Progress.local (fun x -> status := x) in
  Mock_builder.await builder "example1" >>= fun _ ->
  let drain = Cluster_api.Pool_admin.drain ~progress pool "worker-1" in
  Capability.dec_ref progress;
  expect_status_change status "" "Running jobs: 2" >>= fun () ->
  Mock_builder.set builder "example1" @@ Ok "hash";
  expect_status_change status "Running jobs: 2" "Running jobs: 1" >>= fun () ->
  Mock_builder.set builder "example2" @@ Ok "hash";
  r1 >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example1\nJob succeeded\n" result;
  r2 >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example2\nJob succeeded\n" result;
  drain >>= fun () ->
  Cluster_api.Pool_admin.set_active pool "worker-1" true >>= fun () ->
  Mock_builder.set builder "example3" @@ Ok "hash";
  r3 >>= fun result ->
  Alcotest.(check string) "Check job worked" "Building on worker-1\nBuilding example3\nJob succeeded\n" result;
  Capability.dec_ref pool;
  Lwt.return_unit

(* The admin drains a worker, which completes due to the worker crashing. *)
let drain_crash () =
  let builder = Mock_builder.create () in
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
  Lwt_switch.with_switch @@ fun builder_switch ->
  let network_switch = Lwt_switch.create () in
  Mock_builder.run_remote builder ~builder_switch ~network_switch registry;
  let r1 = submit submission_service "example1" in
  Mock_builder.await builder "example1" >>= fun _ ->
  let pool = Cluster_api.Admin.pool admin "pool" in
  let status = ref "" in
  let progress = Cluster_api.Progress.local (fun x -> print_endline x; status := x) in
  let drain = Cluster_api.Pool_admin.drain ~progress pool "worker-1" in
  Capability.dec_ref progress;
  expect_status_change status "" "Running jobs: 1" >>= fun () ->
  (* Worker crashes instead of finishing the job *)
  Lwt_switch.turn_off network_switch >>= fun () ->
  expect_status_change status "Running jobs: 1" "Running jobs: 0" >>= fun () ->
  r1 >>= fun result ->
  Alcotest.(check string) "Check job failed" "Error tailing logs: Disconnected: Connection closed\nFAILED\n" result;
  drain >>= fun () ->
  Capability.dec_ref pool;
  Lwt.return_unit

let worker_info = Alcotest.of_pp Cluster_api.Pool_admin.pp_worker_info

let admin () =
  with_sched @@ fun ~admin ~registry:_ ->
  Cluster_api.Admin.pools admin >>= fun pools ->
  Alcotest.(check (list string)) "Check pools" ["pool"] pools;
  Capability.with_ref (Cluster_api.Admin.pool admin "pool") @@ fun pool ->
  Cluster_api.Pool_admin.show pool >>= fun state ->
  Logs.info (fun f -> f "Pool state:\n%s" state);
  (* Add a paused worker: *)
  Cluster_api.Pool_admin.set_active pool "worker-1" false ~auto_create:true >>= fun () ->
  Cluster_api.Pool_admin.workers pool >>= fun workers ->
  Alcotest.(check (list worker_info)) "Workers" [
    { Cluster_api.Pool_admin.name = "worker-1"; connected = false; active = false };
  ] workers;
  (* Unpause it: *)
  Cluster_api.Pool_admin.set_active pool "worker-1" true >>= fun () ->
  Cluster_api.Pool_admin.workers pool >>= fun workers ->
  Alcotest.(check (list worker_info)) "Workers" [
    { Cluster_api.Pool_admin.name = "worker-1"; connected = false; active = true };
  ] workers;
  (* Forget it: *)
  Cluster_api.Pool_admin.forget pool "worker-1" >>= fun r ->
  assert (r = Ok ());
  Cluster_api.Pool_admin.workers pool >>= fun workers ->
  Alcotest.(check (list worker_info)) "Workers" [] workers;
  Lwt.return_unit

(* Test our mock network. *)
let network () =
  Lwt_switch.with_switch (fun network_switch ->
      let builder = Mock_builder.create () in
      with_sched @@ fun ~admin ~registry ->
      Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
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
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
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
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
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
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
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
  with_sched @@ fun ~admin ~registry:_ ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
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
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
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
  with_sched @@ fun ~admin ~registry:_ ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
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
  with_sched @@ fun ~admin ~registry ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client") @@ fun submission_service ->
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

(* Registering and removing clients. *)
let clients () =
  with_sched @@ fun ~admin ~registry:_ ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client1") @@ fun client1 ->
  Capability.with_ref (Cluster_api.Admin.add_client admin "client2") @@ fun client2 ->
  Cluster_api.Admin.list_clients admin >>= fun clients ->
  Alcotest.(check (list string)) "2 clients" ["client1"; "client2"] clients;
  Cluster_api.Admin.remove_client admin "client1" >>= fun () ->
  Cluster_api.Admin.list_clients admin >>= fun clients ->
  Alcotest.(check (list string)) "2 clients" ["client2"] clients;
  let action = Cluster_api.Submission.docker_build (`Contents "example") in
  let ticket1 = Cluster_api.Submission.submit client1 ~pool:"pool" ~action ~cache_hint:"1" in
  let ticket2 = Cluster_api.Submission.submit client2 ~pool:"pool" ~action ~cache_hint:"1" in
  Capability.await_settled ticket1 >>= fun (_ : _ result) ->
  Capability.await_settled ticket2 >>= fun (_ : _ result) ->
  let pp_err = Fmt.(option ~none:(any "ok")) Capnp_rpc.Exception.pp in
  let problem1 = Fmt.str "%a" pp_err (Capability.problem ticket1) in
  Alcotest.(check string) "Access revoked" "Failed: Access has been revoked" problem1;
  let problem2 = Fmt.str "%a" pp_err (Capability.problem ticket2) in
  Alcotest.(check string) "Access OK" "ok" problem2;
  Capability.dec_ref ticket2;
  Capability.with_ref (Cluster_api.Admin.add_client admin "client2") @@ fun client2b ->
  Capability.await_settled client2b >>= fun (_ : _ result) ->
  let problem = Fmt.str "%a" pp_err (Capability.problem client2b) in
  Alcotest.(check string) "Duplicate user" {|Failed: Client "client2" already registered!|} problem;
  Lwt.try_bind
    (fun () -> Cluster_api.Admin.remove_client admin "client1")
    (fun () -> Alcotest.fail "Should have failed!")
    (fun _ -> Lwt.return_unit)

let test_case ?(expected_warnings=0) name fn =
  Alcotest_lwt.test_case name `Quick @@ fun _ () ->
  let problems = Logs.(warn_count () + err_count ()) in
  fn () >>= fun () ->
  Lwt.pause () >>= fun () ->
  Gc.full_major ();
  Lwt.pause () >|= fun () ->
  let problems' = Logs.(warn_count () + err_count ()) in
  Alcotest.(check int) "Check log for warnings" expected_warnings (problems' - problems)

let () =
  Lwt_main.run begin
    Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna);
    Alcotest_lwt.run ~verbose "ocluster-scheduler" [
      "main", [
        test_case "simple" simple;
        test_case "simple_custom" simple_custom;
        test_case "fails" fails;
        test_case "await_builder" await_builder;
        test_case "already_registered" already_registered ~expected_warnings:1;
        test_case "builder_capacity" builder_capacity;
        test_case "network" network;
        test_case "worker_disconnects" worker_disconnects;
        test_case "client_disconnects" client_disconnects;
        test_case "cancel" cancel;
        test_case "cancel_ticket" cancel_ticket;
        test_case "cancel_ticket_late" cancel_ticket_late;
        test_case "release_ticket" release_ticket;
        test_case "release_ticket_late" release_ticket_late;
        test_case "drain" drain;
        test_case "drain_crash" drain_crash;
        test_case "admin" admin;
        test_case "clients" clients;
      ];
      "plugin", Test_plugin.suite;
      "lwt_retry", Test_lwt_retry.suite;
    ]
  end

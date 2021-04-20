open Lwt.Infix

module Item = struct
  type t = {
    job : string;
    cache_hint : string;
  }

  type cache_hint = string

  let cache_hint t = t.cache_hint

  let cost_estimate _ = Cluster_scheduler.S.{ cached = 2; non_cached = 10 }

  let pp f t = Fmt.string f t.job
end

let job ?(cache_hint="") job = { Item.job; cache_hint }

module Fake_time = struct
  let now = ref 1.0

  let gettimeofday () = !now

  let advance x =
    now := !now +. float x
end

module Inactive_reasons = Cluster_scheduler.Pool.Inactive_reasons
module Pool = Cluster_scheduler.Pool.Make(Item)(Fake_time)

let job_state x =
  match Lwt.state x with
  | Return (Ok item) -> Ok item.Item.job
  | Return (Error `Finished) -> Error "finished"
  | Fail ex -> Error (Printexc.to_string ex)
  | Sleep -> Error "pending"

let pp_result = Fmt.(result ~ok:string ~error:string)
let pp_cancel = Fmt.(result ~ok:(const string "cancelled") ~error:(const string "error"))

let println fmt = Fmt.pr (fmt ^^ "@.")
let print_result = Fmt.pr "%a@." pp_result
let print_pool = Fmt.pr "%a@." Pool.dump

let rec flush_queue w =
  let job = Pool.pop w in
  Lwt.pause () >>= fun () ->
  if Lwt.is_sleeping job then (
    Pool.release w;
    Lwt.return_unit
  ) else (
    println "Flush %a" pp_result (job_state job);
    flush_queue w
  )

let run fn =
  Lwt_main.run @@ begin
    Lwt_unix.yield () >>= fun () ->       (* Ensure we're inside the Lwt mainloop. Lwt.pause behaves strangely otherwise. *)
    Fake_time.now := 1.0;
    fn () >>= fun () ->
    Lwt.pause () >|= fun () ->
    Prometheus.CollectorRegistry.(collect default)
    |> Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output
    |> String.split_on_char '\n'
    |> List.iter (fun line ->
        if Astring.String.is_prefix ~affix:"scheduler_pool_" line then (
          match Astring.String.cut ~sep:"} " line with
          | None -> Fmt.failwith "Bad metrics line: %S" line
          | Some (key, _) when Astring.String.is_infix ~affix:"_total{" key -> ()
          | Some (key, value) ->
            if float_of_string value <> 0.0 then
              Fmt.failwith "Non-zero metric after test: %s=%s" key value
        )
      )
  end

let with_test_db fn =
  run @@ fun () ->
  let db = Sqlite3.db_open ":memory:" in
  Lwt.finalize
    (fun () -> fn (Cluster_scheduler.Pool.Dao.init db))
    (fun () -> if Sqlite3.db_close db then Lwt.return_unit else failwith "close: DB busy!")

let submit ~urgent client job =
  let (_ : Pool.ticket) = Pool.Client.submit ~urgent client job in
  ()

(* Assign three jobs to two workers. *)
let%expect_test "simple" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"simple" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  let user = Pool.client pool ~client_id:"u1" in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  let w1a = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  submit user ~urgent:false @@ job "job1";
  submit user ~urgent:false @@ job "job2";
  submit user ~urgent:false @@ job "job3";
  Lwt.pause () >>= fun () ->
  (* Worker 1 / job 1 *)
  print_result (job_state w1a);
  [%expect{| job1 |}];
  (* Worker 2 / job 1 *)
  print_result (job_state w2a);
  [%expect{| job2 |}];
  Pool.release w2;
  flush_queue w1 >|= fun () ->
  [%expect {| Flush job3 |}]

(* Bias assignment towards workers that have things cached. *)
let%expect_test "cached_scheduling" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"cached_scheduling" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  let w1a = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  (* Workers ready *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (ready) [worker-2 worker-1]
    registered:
      worker-1 (0): []
      worker-2 (0): []
    clients:
    cached:
  |}];
  let user = Pool.client pool ~client_id:"u1" in
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job2" ~cache_hint:"b";
  submit user ~urgent:false @@ job "job3" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job4" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job5" ~cache_hint:"c";
  (* Jobs queued *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (backlog) [u1:job5@12s u1:job4@11s u1:job3@10s]
    registered:
      worker-1 (10): [u1:job1@0s(10)]
      worker-2 (10): [u1:job2@5s(10)]
    clients: u1(2)+17s
    cached: a: [worker-1], b: [worker-2]
  |}];
  Lwt.pause () >>= fun () ->
  (* Jobs started *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (backlog) [u1:job5@12s u1:job4@11s u1:job3@10s]
    registered:
      worker-1 (0): []
      worker-2 (0): []
    clients: u1(2)+17s
    cached: a: [worker-1], b: [worker-2]
  |}];
  (* Worker 1 / job 1 *)
  print_result (job_state w1a);
  [%expect{| job1 |}];
  (* Worker 2 / job 1 *)
  print_result (job_state w2a);
  [%expect{| job2 |}];
  (* Worker 2 asks for another job, but the next two jobs would be better done on worker-1. *)
  let w2b = Pool.pop w2 in
  (* Jobs 3 and 4 assigned to worker-1 *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (backlog) []
    registered:
      worker-1 (4): [u1:job4@11s(2) u1:job3@10s(2)]
      worker-2 (0): []
    clients: u1(2)+17s
    cached: a: [worker-1], b: [worker-2], c: [worker-2]
  |}];
  (* Worker 2 / job 2 *)
  print_result (job_state w2b);
  [%expect{| job5 |}];
  (* Worker 1 leaves. Its two queued jobs get reassigned. *)
  let w2c = Pool.pop w2 in
  (* Worker 2 / job 3 *)
  print_result (job_state w2c);
  [%expect{| pending |}];
  Logs.info (fun f -> f "@[<v2>w1 about to leave:@,%a@]" Pool.dump pool);
  Pool.release w1;
  Lwt.pause () >>= fun () ->
  (* Worker-1's jobs reassigned *)
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) [u1:job4@11s]
    registered:
      worker-2 (0): []
    clients: u1(2)+17s
    cached: a: [worker-1; worker-2], b: [worker-2], c: [worker-2]
  |}];
  (* Worker 2 / job 3 *)
  print_result (job_state w2c);
  [%expect{| job3 |}];
  let w2d = Pool.pop w2 in
  (* Worker 2 / job 4 *)
  print_result (job_state w2d);
  [%expect{| job4 |}];
  Fake_time.advance 20;
  Pool.release w2;
  (* Idle *)
  print_pool pool;
  [%expect {|
    capacity: 0
    queue: (backlog) []
    registered:
    clients: u1(2)
    cached: a: [worker-1; worker-2], b: [worker-2], c: [worker-2]
  |}];
  Lwt.return_unit

(* Bias assignment towards workers that have things cached, but not so much that it takes longer. *)
let%expect_test "unbalanced" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"unbalanced" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  let user = Pool.client pool ~client_id:"u1" in
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job3" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job4" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job5" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job6" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job7" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job8" ~cache_hint:"a";
  let _ = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  Lwt.pause () >>= fun () ->
  (* Worker-2 got jobs eventually *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (backlog) []
    registered:
      worker-1 (12): [u1:job7@10s(2) u1:job6@9s(2) u1:job5@8s(2) u1:job4@7s(2)
                      u1:job3@6s(2) u1:job2@5s(2)]
      worker-2 (0): []
    clients: u1(2)+12s
    cached: a: [worker-1; worker-2]
  |}];
  (* Worker 2 / job 1 *)
  print_result (job_state w2a);
  [%expect{| job8 |}];
  Pool.release w1;
  flush_queue w2 >|= fun () ->
  [%expect {|
    Flush job2
    Flush job3
    Flush job4
    Flush job5
    Flush job6
    Flush job7
  |}]

(* There are no workers available sometimes. *)
let%expect_test "no_workers" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"no_workers" in
  let user = Pool.client pool ~client_id:"u1" in
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  let _ = Pool.pop w1 in
  Pool.release w1;
  Lwt.pause () >>= fun () ->
  (* Worker-1 gone *)
  print_pool pool;
  [%expect{|
    capacity: 0
    queue: (backlog) [u1:job2@10s]
    registered:
    clients: u1(1)+12s
    cached: a: [worker-1]
  |}];
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  flush_queue w1 >|= fun () ->
  [%expect {| Flush job2 |}]

(* We remember cached locations across restarts. *)
let%expect_test "persist" =
  with_test_db @@ fun db ->
  begin
    let pool = Pool.create ~db ~name:"persist" in
    (* worker-1 handles job1 *)
    let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
    Pool.set_active w1 true ~reason:Inactive_reasons.worker;
    let user = Pool.client pool ~client_id:"u1" in
    submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
    let _ = Pool.pop w1 in
    Pool.release w1;
  end;
  (* Create a new instance of the scheduler with the same db. *)
  let pool = Pool.create ~db ~name:"persist" in
  let user = Pool.client pool ~client_id:"u1" in
  (* Worker 2 registers first, and so would normally get the first job: *)
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  let w2a = Pool.pop w2 in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  let w1a = Pool.pop w1 in
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  Lwt.pause () >>= fun () ->
  (* Worker 1 gets the job *)
  print_result (job_state w1a);
  [%expect{| job2 |}];
  (* Worker 2 doesn't *)
  print_result (job_state w2a);
  [%expect{| pending |}];
  Pool.release w1;
  Pool.release w2;
  Lwt.pause ()

(* Urgent jobs go first. *)
let%expect_test "urgent" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"urgent" in
  let user = Pool.client pool ~client_id:"u1" in
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:true  @@ job "job2" ~cache_hint:"a";
  submit user ~urgent:true  @@ job "job3" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job4" ~cache_hint:"b";
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  let w1a = Pool.pop w1 in
  (* Worker 1 / job 1 *)
  print_result (job_state w1a);
  [%expect{| job2 |}];
  (* Worker 2 joins and gets job 4, due to the cache hints: *)
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  let w2a = Pool.pop w2 in
  (* Worker 2 / job 1 *)
  print_result (job_state w2a);
  [%expect{| job4 |}];
  (* Worker 1 leaves. Jobs 1 and 3 get pushed back, keeping their ugency level. *)
  Pool.release w1;
  (* Worker-1 gone *)
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) [u1:job1@0s u1:job3@12s+urgent]
    registered:
      worker-2 (0): []
    clients: u1(1)+24s
    cached: a: [worker-1], b: [worker-2]
  |}];
  (* Urgent job 5 goes ahead of non-urgent job 1, but behind the existing urgent job 3. *)
  submit user ~urgent:true @@ job "job5" ~cache_hint:"b";
  flush_queue w2 >|= fun () ->
  [%expect {|
    Flush job3
    Flush job5
    Flush job1
  |}]

(* Urgent jobs go first on worker queues too. *)
let%expect_test "urgent_worker" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"urgent-worker" in
  let user = Pool.client pool ~client_id:"u1" in
  submit user ~urgent:true  @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job3" ~cache_hint:"b";
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  let w1a = Pool.pop w1 in
  (* Worker 1 / job 1 *)
  print_result (job_state w1a);
  [%expect{| job1 |}];
  (* Worker 2 joins and gets job 3, due to the cache hints, putting job2 on
     worker 1's queue. *)
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  let w2a = Pool.pop w2 in
  (* Worker 2 / job 1 *)
  print_result (job_state w2a);
  [%expect{| job3 |}];
  (* Worker-1 queue has job 2 queued *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (backlog) []
    registered:
      worker-1 (2): [u1:job2@10s(2)]
      worker-2 (0): []
    clients: u1(1)+22s
    cached: a: [worker-1], b: [worker-2]
  |}];
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:true  @@ job "job4" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job5" ~cache_hint:"b";
  submit user ~urgent:true  @@ job "job6" ~cache_hint:"a";
  let w2b = Pool.pop w2 in
  (* Worker 2 / job 2 *)
  print_result (job_state w2b);
  [%expect{| job5 |}];
  (* Worker-1 gets job4 first *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (backlog) []
    registered:
      worker-1 (6): [u1:job2@10s(2) u1:job6@24s(2+urgent) u1:job4@22s(2+urgent)]
      worker-2 (0): []
    clients: u1(2)+25s
    cached: a: [worker-1], b: [worker-2]
  |}];
  flush_queue w1 >|= fun () ->
  [%expect {|
    Flush job4
    Flush job6
    Flush job2
  |}]

(* Workers can be paused and resumed. *)
let%expect_test "inactive" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"inactive" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  let w1a = Pool.pop w1 in
  let user = Pool.client pool ~client_id:"u1" in
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  let w2a = Pool.pop w2 in
  Lwt.pause () >>= fun () ->
  (* Both jobs assigned to Worker-1 *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (ready) [worker-2]
    registered:
      worker-1 (2): [u1:job2@10s(2)]
      worker-2 (0): []
    clients: u1(1)+12s
    cached: a: [worker-1]
  |}];
  (* Deactivate worker-1. Its job is reassigned. *)
  Pool.set_active w1 false ~reason:Inactive_reasons.worker;
  (* Job reassigned to Worker-2 *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (ready) []
    registered:
      worker-1 (0): (inactive: worker pause)
      worker-2 (10): [u1:job2@10s(10)]
    clients: u1(1)+12s
    cached: a: [worker-1; worker-2]
  |}];
  Lwt.pause () >>= fun () ->
  (* Worker 1 / job 1 *)
  print_result (job_state w1a);
  [%expect{| job1 |}];
  (* Worker 2 / job 2 *)
  print_result (job_state w2a);
  [%expect{| job2 |}];
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:false @@ job "job3" ~cache_hint:"a";
  (* Deactivate worker-2. *)
  Pool.set_active w2 false ~reason:Inactive_reasons.worker;
  (* Job unassigned *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (backlog) [u1:job3@12s]
    registered:
      worker-1 (0): (inactive: worker pause)
      worker-2 (0): (inactive: worker pause)
    clients: u1(2)+13s
    cached: a: [worker-1; worker-2]
  |}];
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  Pool.release w1;
  flush_queue w2 >|= fun () ->
  [%expect {| Flush job3 |}]

(* The user cancels while the item is assigned. *)
let%expect_test "cancel_worker_queue" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"cancel_worker_queue" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  let w1a = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  let user = Pool.client pool ~client_id:"u1" in
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  Lwt.pause () >>= fun () ->
  let j2 = Pool.Client.submit user ~urgent:false @@ job "job2" ~cache_hint:"a" in
  let j3 = Pool.Client.submit user ~urgent:false @@ job "job3" ~cache_hint:"a" in
  let j4 = Pool.Client.submit user ~urgent:false @@ job "job4" ~cache_hint:"b" in
  (* job4 cancelled *)
  println "%a" pp_cancel (Pool.Client.cancel user j4);
  [%expect{| cancelled |}];
  (* Jobs assigned *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (ready) []
    registered:
      worker-1 (4): [u1:job3@6s(2) u1:job2@5s(2)]
      worker-2 (0): []
    clients: u1(2)+12s
    cached: a: [worker-1], b: [worker-2]
  |}];
  (* job2 cancelled *)
  println "%a" pp_cancel (Pool.Client.cancel user j2);
  [%expect{| cancelled |}];
  (* Job2 cancelled *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (ready) []
    registered:
      worker-1 (2): [u1:job3@6s(2)]
      worker-2 (0): []
    clients: u1(2)+12s
    cached: a: [worker-1], b: [worker-2]
  |}];
  Pool.release w2;
  Pool.set_active w1 false ~reason:Inactive_reasons.worker;
  (* Job3 pushed back *)
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) [u1:job3@6s]
    registered:
      worker-1 (0): (inactive: worker pause)
    clients: u1(2)+12s
    cached: a: [worker-1], b: [worker-2]
  |}];
  (* job3 cancelled *)
  println "%a" pp_cancel (Pool.Client.cancel user j3);
  [%expect{| cancelled |}];
  Pool.release w1;
  (* Job3 cancelled *)
  print_pool pool;
  [%expect{|
    capacity: 0
    queue: (backlog) []
    registered:
    clients: u1(2)+12s
    cached: a: [worker-1], b: [worker-2]
  |}];
  (* Finish worker-1 *)
  print_result (job_state w1a);
  [%expect{| job1 |}];
  (* Finish worker-2 *)
  print_result (job_state w2a);
  [%expect {| pending |}];
  Lwt.return_unit

(* A worker is marked as inactive. Its items go back on the main queue. *)
let%expect_test "push_back" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"push_back" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  let _w1a = Pool.pop w1 in
  let _w2a = Pool.pop w2 in
  let user = Pool.client pool ~client_id:"u1" in
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  Lwt.pause () >>= fun () ->
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job3" ~cache_hint:"a";
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  Lwt.pause () >>= fun () ->
  (* Jobs assigned *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (ready) [worker-2]
    registered:
      worker-1 (4): [u1:job3@6s(2) u1:job2@5s(2)]
      worker-2 (0): []
    clients: u1(2)+7s
    cached: a: [worker-1]
  |}];
  Pool.release w2;
  Pool.set_active w1 false ~reason:Inactive_reasons.worker;
  (* Jobs pushed back *)
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) [u1:job3@6s u1:job2@5s]
    registered:
      worker-1 (0): (inactive: worker pause)
    clients: u1(2)+7s
    cached: a: [worker-1]
  |}];
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  flush_queue w1 >|= fun () ->
  [%expect {|
    Flush job2
    Flush job3
  |}]

(* Two clients share the cluster. *)
let%expect_test "fairness" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"fairness" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  let alice = Pool.client pool ~client_id:"alice" in
  let bob = Pool.client pool ~client_id:"bob" in
  Pool.Client.set_rate alice 2.0;
  Pool.Client.set_rate bob 2.0;
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  let w1a = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  submit alice ~urgent:false @@ job "a1";
  submit alice ~urgent:false @@ job "a2";
  submit alice ~urgent:false @@ job "a3";
  (* Alice's jobs a1 and a2 have already started running on the two machines,
     and a3 is queued. Bob now submits some jobs. *)
  submit bob ~urgent:false @@ job "b1";
  submit bob ~urgent:false @@ job "b2";
  submit bob ~urgent:false @@ job "b3";
  Lwt.pause () >>= fun () ->
  (* Worker 1 / job 1 *)
  print_result (job_state w1a);
  [%expect{| a1 |}];
  (* Worker 2 / job 1 *)
  print_result (job_state w2a);
  [%expect{| a2 |}];
  (* Bob's jobs aren't all last *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (backlog) [bob:b3@10s alice:a3@10s bob:b2@5s bob:b1@0s]
    registered:
      worker-1 (0): []
      worker-2 (0): []
    clients: alice(2)+15s bob(2)+15s
    cached: : [worker-1; worker-2]
  |}];
  Pool.release w2;
  flush_queue w1 >|= fun () ->
  [%expect {|
    Flush b1
    Flush b2
    Flush a3
    Flush b3
  |}]

(* Two clients with different rates share the cluster. *)
let%expect_test "fairness_rates" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"fairness_rates" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  let alice = Pool.client pool ~client_id:"alice" in
  let bob = Pool.client pool ~client_id:"bob" in
  Pool.Client.set_rate alice 5.0;
  Pool.Client.set_rate bob 1.0;
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  Pool.set_active w2 true ~reason:Inactive_reasons.worker;
  let w1a = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  (* Alice submits 30s worth of work, but using 5 machines expects to take 6s. *)
  submit alice ~urgent:false @@ job "a1";
  submit alice ~urgent:false @@ job "a2";
  submit alice ~urgent:false @@ job "a3";
  (* Alice's jobs a1 and a2 have already started running on the two machines,
     and a3 is queued. Bob now submits some jobs. It's the same amount of work,
     but with the lower rate, Bob expects his jobs to take 30s. *)
  submit bob ~urgent:false @@ job "b1";
  submit bob ~urgent:false @@ job "b2";
  submit bob ~urgent:false @@ job "b3";
  Lwt.pause () >>= fun () ->
  (* Worker 1 / job 1 *)
  print_result (job_state w1a);
  [%expect{| a1 |}];
  (* Worker 2 / job 1 *)
  print_result (job_state w2a);
  [%expect{| a2 |}];
  (* Bob's jobs aren't all last *)
  print_pool pool;
  [%expect{|
    capacity: 2
    queue: (backlog) [bob:b3@20s bob:b2@10s alice:a3@4s bob:b1@0s]
    registered:
      worker-1 (0): []
      worker-2 (0): []
    clients: alice(5)+6s bob(1)+30s
    cached: : [worker-1; worker-2]
  |}];
  Pool.release w2;
  flush_queue w1 >|= fun () ->
  [%expect {|
    Flush b1
    Flush a3
    Flush b2
    Flush b3
  |}]

(* Keep track of inactive reasons. *)
let%expect_test "inactive" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"simple" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  (* Workers start paused: *)
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) []
    registered:
      worker-1 (0): (inactive: worker pause)
    clients:
    cached: |}];
  (* Now also paused by admin: *)
  Pool.set_active w1 false ~reason:Inactive_reasons.admin_pause;
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) []
    registered:
      worker-1 (0): (inactive: worker pause, admin pause)
    clients:
    cached: |}];
  (* Worker unpauses, but is still inactive: *)
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) []
    registered:
      worker-1 (0): (inactive: admin pause)
    clients:
    cached: |}];
  (* Admin also unpauses: *)
  Pool.set_active w1 true ~reason:Inactive_reasons.admin_pause;
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) []
    registered:
      worker-1 (0): []
    clients:
    cached: |}];
  (* Shutdown: *)
  Pool.shutdown w1;
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) []
    registered:
      worker-1 (0): (inactive: shutting down)
    clients:
    cached: |}];
  Lwt.return_unit

(* The admin-paused state is persisted over worker restarts. *)
let%expect_test "persist_pause" =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"simple" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  (* The admin pauses it: *)
  Pool.set_active w1 false ~reason:Inactive_reasons.admin_pause;
  Pool.shutdown w1;
  (* Worker is now self-paused, admin-paused and shutting down: *)
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) []
    registered:
      worker-1 (0): (inactive: worker pause, admin pause, shutting down)
    clients:
    cached: |}];
  (* The worker unpauses itself, but this will not persist: *)
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  Pool.release w1;
  (* On reconnect, it is still admin-paused (and now worker-paused again too): *)
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) []
    registered:
      worker-1 (0): (inactive: worker pause, admin pause)
    clients:
    cached: |}];
  (* The admin unpauses it: *)
  Pool.set_active w1 true ~reason:Inactive_reasons.admin_pause;
  Pool.release w1;
  (* On reconnect, it is still admin unpaused: *)
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true ~reason:Inactive_reasons.worker;
  print_pool pool;
  [%expect{|
    capacity: 1
    queue: (backlog) []
    registered:
      worker-1 (0): []
    clients:
    cached: |}];
  Lwt.return_unit

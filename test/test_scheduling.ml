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

module Pool = Cluster_scheduler.Pool.Make(Item)(Fake_time)

let job_state x =
  match Lwt.state x with
  | Return (Ok item) -> Ok item.Item.job
  | Return (Error `Finished) -> Error "finished"
  | Fail ex -> Error (Printexc.to_string ex)
  | Sleep -> Error "pending"

let pop_result = Alcotest.(result string string)

let flush_queue w ~expect =
  let rec aux = function
    | [] -> Pool.release w; Lwt.return_unit
    | x :: xs ->
      let job = Pool.pop w in
      Lwt.pause () >>= fun () ->
      Alcotest.(check pop_result) ("Flush " ^ x) (Ok x) (job_state job);
      aux xs
  in
  aux expect

let with_test_db fn =
  let db = Sqlite3.db_open ":memory:" in
  Lwt.finalize
    (fun () -> fn (Cluster_scheduler.Pool.Dao.init db))
    (fun () -> if Sqlite3.db_close db then Lwt.return_unit else failwith "close: DB busy!")

let submit ~urgent client job =
  let (_ : Pool.ticket) = Pool.Client.submit ~urgent client job in
  ()

(* Assign three jobs to two workers. *)
let simple () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"simple" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  let user = Pool.client pool ~client_id:"u1" in
  Pool.set_active w1 true;
  Pool.set_active w2 true;
  let w1a = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  submit user ~urgent:false @@ job "job1";
  submit user ~urgent:false @@ job "job2";
  submit user ~urgent:false @@ job "job3";
  Lwt.pause () >>= fun () ->
  Alcotest.(check pop_result) "Worker 1 / job 1" (Ok "job1") (job_state w1a);
  Alcotest.(check pop_result) "Worker 2 / job 1" (Ok "job2") (job_state w2a);
  Pool.release w2;
  flush_queue w1 ~expect:["job3"]

(* Bias assignment towards workers that have things cached. *)
let cached_scheduling () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"cached_scheduling" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true;
  Pool.set_active w2 true;
  let w1a = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  Alcotest.(check string) "Workers ready" "\
    capacity: 2\n\
    queue: (ready) [worker-2 worker-1]\n\
    registered:\n\
    \  worker-1 (0): []\n\
    \  worker-2 (0): []\n\
    clients: \n\
    cached: \n" (Fmt.to_to_string Pool.dump pool);
  let user = Pool.client pool ~client_id:"u1" in
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job2" ~cache_hint:"b";
  submit user ~urgent:false @@ job "job3" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job4" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job5" ~cache_hint:"c";
  Alcotest.(check string) "Jobs queued" "\
    capacity: 2\n\
    queue: (backlog) [u1:job5@12s u1:job4@11s u1:job3@10s]\n\
    registered:\n\
    \  worker-1 (10): [u1:job1@0s(10)]\n\
    \  worker-2 (10): [u1:job2@5s(10)]\n\
    clients: u1(2)+17s\n\
    cached: a: [worker-1], b: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Lwt.pause () >>= fun () ->
  Alcotest.(check string) "Jobs started" "\
    capacity: 2\n\
    queue: (backlog) [u1:job5@12s u1:job4@11s u1:job3@10s]\n\
    registered:\n\
    \  worker-1 (0): []\n\
    \  worker-2 (0): []\n\
    clients: u1(2)+17s\n\
    cached: a: [worker-1], b: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Alcotest.(check pop_result) "Worker 1 / job 1" (Ok "job1") (job_state w1a);
  Alcotest.(check pop_result) "Worker 2 / job 1" (Ok "job2") (job_state w2a);
  (* Worker 2 asks for another job, but the next two jobs would be better done on worker-1. *)
  let w2b = Pool.pop w2 in
  Alcotest.(check string) "Jobs 3 and 4 assigned to worker-1" "\
    capacity: 2\n\
    queue: (backlog) []\n\
    registered:\n\
    \  worker-1 (4): [u1:job4@11s(2) u1:job3@10s(2)]\n\
    \  worker-2 (0): []\n\
    clients: u1(2)+17s\n\
    cached: a: [worker-1], b: [worker-2], c: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Alcotest.(check pop_result) "Worker 2 / job 2" (Ok "job5") (job_state w2b);
  (* Worker 1 leaves. Its two queued jobs get reassigned. *)
  let w2c = Pool.pop w2 in
  Alcotest.(check pop_result) "Worker 2 / job 3" (Error "pending") (job_state w2c);
  Logs.info (fun f -> f "@[<v2>w1 about to leave:@,%a@]" Pool.dump pool);
  Pool.release w1;
  Lwt.pause () >>= fun () ->
  Alcotest.(check string) "Worker-1's jobs reassigned" "\
    capacity: 1\n\
    queue: (backlog) [u1:job4@11s]\n\
    registered:\n\
    \  worker-2 (0): []\n\
    clients: u1(2)+17s\n\
    cached: a: [worker-1; worker-2], b: [worker-2], c: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Alcotest.(check pop_result) "Worker 2 / job 3" (Ok "job3") (job_state w2c);
  let w2d = Pool.pop w2 in
  Alcotest.(check pop_result) "Worker 2 / job 4" (Ok "job4") (job_state w2d);
  Fake_time.advance 20;
  Pool.release w2;
  Alcotest.(check string) "Idle" "\
    capacity: 0\n\
    queue: (backlog) []\n\
    registered:\n\
    clients: u1(2)\n\
    cached: a: [worker-1; worker-2], b: [worker-2], c: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Lwt.return_unit

(* Bias assignment towards workers that have things cached, but not so much that it takes longer. *)
let unbalanced () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"unbalanced" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true;
  Pool.set_active w2 true;
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
  Alcotest.(check string) "Worker-2 got jobs eventually" "\
    capacity: 2\n\
    queue: (backlog) []\n\
    registered:\n\
    \  worker-1 (12): [u1:job7@10s(2) u1:job6@9s(2) u1:job5@8s(2) u1:job4@7s(2)\n\
    \                  u1:job3@6s(2) u1:job2@5s(2)]\n\
    \  worker-2 (0): []\n\
    clients: u1(2)+12s\n\
    cached: a: [worker-1; worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Alcotest.(check pop_result) "Worker 2 / job 1" (Ok "job8") (job_state w2a);
  Pool.release w1;
  flush_queue w2 ~expect:["job2"; "job3"; "job4"; "job5"; "job6"; "job7"]

(* There are no workers available sometimes. *)
let no_workers () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"no_workers" in
  let user = Pool.client pool ~client_id:"u1" in
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true;
  let _ = Pool.pop w1 in
  Pool.release w1;
  Lwt.pause () >>= fun () ->
  Alcotest.(check string) "Worker-1 gone" "\
    capacity: 0\n\
    queue: (backlog) [u1:job2@10s]\n\
    registered:\n\
    clients: u1(1)+12s\n\
    cached: a: [worker-1]\n" (Fmt.to_to_string Pool.dump pool);
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true;
  flush_queue w1 ~expect:["job2"]

(* We remember cached locations across restarts. *)
let persist () =
  with_test_db @@ fun db ->
  begin
    let pool = Pool.create ~db ~name:"persist" in
    (* worker-1 handles job1 *)
    let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
    Pool.set_active w1 true;
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
  Pool.set_active w2 true;
  let w2a = Pool.pop w2 in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true;
  let w1a = Pool.pop w1 in
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  Lwt.pause () >>= fun () ->
  Alcotest.(check pop_result) "Worker 1 gets the job" (Ok "job2") (job_state w1a);
  Alcotest.(check pop_result) "Worker 2 doesn't" (Error "pending") (job_state w2a);
  Pool.release w1;
  Pool.release w2;
  Lwt.pause () >>= fun () ->
  Lwt.return_unit

(* Urgent jobs go first. *)
let urgent () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"urgent" in
  let user = Pool.client pool ~client_id:"u1" in
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:true  @@ job "job2" ~cache_hint:"a";
  submit user ~urgent:true  @@ job "job3" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job4" ~cache_hint:"b";
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true;
  let w1a = Pool.pop w1 in
  Alcotest.(check pop_result) "Worker 1 / job 1" (Ok "job2") (job_state w1a);
  (* Worker 2 joins and gets job 4, due to the cache hints: *)
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w2 true;
  let w2a = Pool.pop w2 in
  Alcotest.(check pop_result) "Worker 2 / job 1" (Ok "job4") (job_state w2a);
  (* Worker 1 leaves. Jobs 1 and 3 get pushed back, keeping their ugency level. *)
  Pool.release w1;
  Alcotest.(check string) "Worker-1 gone" "\
    capacity: 1\n\
    queue: (backlog) [u1:job1@0s u1:job3@12s+urgent]\n\
    registered:\n\
    \  worker-2 (0): []\n\
    clients: u1(1)+24s\n\
    cached: a: [worker-1], b: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  (* Urgent job 5 goes ahead of non-urgent job 1, but behind the existing urgent job 3. *)
  submit user ~urgent:true @@ job "job5" ~cache_hint:"b";
  flush_queue w2 ~expect:["job3"; "job5"; "job1"]

(* Urgent jobs go first on worker queues too. *)
let urgent_worker () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"urgent-worker" in
  let user = Pool.client pool ~client_id:"u1" in
  submit user ~urgent:true  @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job3" ~cache_hint:"b";
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true;
  let w1a = Pool.pop w1 in
  Alcotest.(check pop_result) "Worker 1 / job 1" (Ok "job1") (job_state w1a);
  (* Worker 2 joins and gets job 3, due to the cache hints, putting job2 on
     worker 1's queue. *)
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w2 true;
  let w2a = Pool.pop w2 in
  Alcotest.(check pop_result) "Worker 2 / job 1" (Ok "job3") (job_state w2a);
  Alcotest.(check string) "Worker-1 queue has job 2 queued" "\
    capacity: 2\n\
    queue: (backlog) []\n\
    registered:\n\
    \  worker-1 (2): [u1:job2@10s(2)]\n\
    \  worker-2 (0): []\n\
    clients: u1(1)+22s\n\
    cached: a: [worker-1], b: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:true  @@ job "job4" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job5" ~cache_hint:"b";
  submit user ~urgent:true  @@ job "job6" ~cache_hint:"a";
  let w2b = Pool.pop w2 in
  Alcotest.(check pop_result) "Worker 2 / job 2" (Ok "job5") (job_state w2b);
  Alcotest.(check string) "Worker-1 gets job4 first" "\
    capacity: 2\n\
    queue: (backlog) []\n\
    registered:\n\
    \  worker-1 (6): [u1:job2@10s(2) u1:job6@24s(2+urgent) u1:job4@22s(2+urgent)]\n\
    \  worker-2 (0): []\n\
    clients: u1(2)+25s\n\
    cached: a: [worker-1], b: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  flush_queue w1 ~expect:["job4"; "job6"; "job2"]

(* Workers can be paused and resumed. *)
let inactive () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"inactive" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true;
  let w1a = Pool.pop w1 in
  let user = Pool.client pool ~client_id:"u1" in
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w2 true;
  let w2a = Pool.pop w2 in
  Lwt.pause () >>= fun () ->
  Alcotest.(check string) "Both jobs assigned to Worker-1" "\
    capacity: 2\n\
    queue: (ready) [worker-2]\n\
    registered:\n\
    \  worker-1 (2): [u1:job2@10s(2)]\n\
    \  worker-2 (0): []\n\
    clients: u1(1)+12s\n\
    cached: a: [worker-1]\n" (Fmt.to_to_string Pool.dump pool);
  (* Deactivate worker-1. Its job is reassigned. *)
  Pool.set_active w1 false;
  Alcotest.(check string) "Job reassigned to Worker-2" "\
    capacity: 2\n\
    queue: (ready) []\n\
    registered:\n\
    \  worker-1 (0): (inactive)\n\
    \  worker-2 (10): [u1:job2@10s(10)]\n\
    clients: u1(1)+12s\n\
    cached: a: [worker-1; worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Lwt.pause () >>= fun () ->
  Alcotest.(check pop_result) "Worker 1 / job 1" (Ok "job1") (job_state w1a);
  Alcotest.(check pop_result) "Worker 2 / job 2" (Ok "job2") (job_state w2a);
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:false @@ job "job3" ~cache_hint:"a";
  (* Deactivate worker-2. *)
  Pool.set_active w2 false;
  Alcotest.(check string) "Job unassigned" "\
    capacity: 2\n\
    queue: (backlog) [u1:job3@12s]\n\
    registered:\n\
    \  worker-1 (0): (inactive)\n\
    \  worker-2 (0): (inactive)\n\
    clients: u1(2)+13s\n\
    cached: a: [worker-1; worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Pool.set_active w2 true;
  Pool.release w1;
  flush_queue w2 ~expect:["job3"]

(* The user cancels while the item is assigned. *)
let cancel_worker_queue () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"cancel_worker_queue" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true;
  Pool.set_active w2 true;
  let w1a = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  let user = Pool.client pool ~client_id:"u1" in
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  Lwt.pause () >>= fun () ->
  let j2 = Pool.Client.submit user ~urgent:false @@ job "job2" ~cache_hint:"a" in
  let j3 = Pool.Client.submit user ~urgent:false @@ job "job3" ~cache_hint:"a" in
  let j4 = Pool.Client.submit user ~urgent:false @@ job "job4" ~cache_hint:"b" in
  Pool.Client.cancel user j4 |> Alcotest.(check (result pass reject)) "job4 cancelled" (Ok ());
  Alcotest.(check string) "Jobs assigned" "\
    capacity: 2\n\
    queue: (ready) []\n\
    registered:\n\
    \  worker-1 (4): [u1:job3@6s(2) u1:job2@5s(2)]\n\
    \  worker-2 (0): []\n\
    clients: u1(2)+12s\n\
    cached: a: [worker-1], b: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Pool.Client.cancel user j2 |> Alcotest.(check (result pass reject)) "job2 cancelled" (Ok ());
  Alcotest.(check string) "Job2 cancelled" "\
    capacity: 2\n\
    queue: (ready) []\n\
    registered:\n\
    \  worker-1 (2): [u1:job3@6s(2)]\n\
    \  worker-2 (0): []\n\
    clients: u1(2)+12s\n\
    cached: a: [worker-1], b: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Pool.release w2;
  Pool.set_active w1 false;
  Alcotest.(check string) "Job3 pushed back" "\
    capacity: 1\n\
    queue: (backlog) [u1:job3@6s]\n\
    registered:\n\
    \  worker-1 (0): (inactive)\n\
    clients: u1(2)+12s\n\
    cached: a: [worker-1], b: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Pool.Client.cancel user j3 |> Alcotest.(check (result pass reject)) "job3 cancelled" (Ok ());
  Pool.release w1;
  Alcotest.(check string) "Job3 cancelled" "\
    capacity: 0\n\
    queue: (backlog) []\n\
    registered:\n\
    clients: u1(2)+12s\n\
    cached: a: [worker-1], b: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Alcotest.(check pop_result) "Finish worker-1" (Ok "job1") (job_state w1a);
  Alcotest.(check pop_result) "Finish worker-2" (Error "pending") (job_state w2a);
  Lwt.return_unit

(* A worker is marked as inactive. Its items go back on the main queue. *)
let push_back () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"push_back" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  Pool.set_active w1 true;
  let _w1a = Pool.pop w1 in
  let _w2a = Pool.pop w2 in
  let user = Pool.client pool ~client_id:"u1" in
  Pool.Client.set_rate user 2.0;
  submit user ~urgent:false @@ job "job1" ~cache_hint:"a";
  Lwt.pause () >>= fun () ->
  submit user ~urgent:false @@ job "job2" ~cache_hint:"a";
  submit user ~urgent:false @@ job "job3" ~cache_hint:"a";
  Pool.set_active w2 true;
  Lwt.pause () >>= fun () ->
  Alcotest.(check string) "Jobs assigned" "\
    capacity: 2\n\
    queue: (ready) [worker-2]\n\
    registered:\n\
    \  worker-1 (4): [u1:job3@6s(2) u1:job2@5s(2)]\n\
    \  worker-2 (0): []\n\
    clients: u1(2)+7s\n\
    cached: a: [worker-1]\n" (Fmt.to_to_string Pool.dump pool);
  Pool.release w2;
  Pool.set_active w1 false;
  Alcotest.(check string) "Jobs pushed back" "\
    capacity: 1\n\
    queue: (backlog) [u1:job3@6s u1:job2@5s]\n\
    registered:\n\
    \  worker-1 (0): (inactive)\n\
    clients: u1(2)+7s\n\
    cached: a: [worker-1]\n" (Fmt.to_to_string Pool.dump pool);
  Pool.set_active w1 true;
  flush_queue w1 ~expect:["job2"; "job3"]

(* Two clients share the cluster. *)
let fairness () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"fairness" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  let alice = Pool.client pool ~client_id:"alice" in
  let bob = Pool.client pool ~client_id:"bob" in
  Pool.Client.set_rate alice 2.0;
  Pool.Client.set_rate bob 2.0;
  Pool.set_active w1 true;
  Pool.set_active w2 true;
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
  Alcotest.(check pop_result) "Worker 1 / job 1" (Ok "a1") (job_state w1a);
  Alcotest.(check pop_result) "Worker 2 / job 1" (Ok "a2") (job_state w2a);
  Alcotest.(check string) "Bob's jobs aren't all last" "\
    capacity: 2\n\
    queue: (backlog) [bob:b3@10s alice:a3@10s bob:b2@5s bob:b1@0s]\n\
    registered:\n\
    \  worker-1 (0): []\n\
    \  worker-2 (0): []\n\
    clients: alice(2)+15s bob(2)+15s\n\
    cached: : [worker-1; worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Pool.release w2;
  flush_queue w1 ~expect:["b1"; "b2"; "a3"; "b3"]

(* Two clients with different rates share the cluster. *)
let fairness_rates () =
  with_test_db @@ fun db ->
  let pool = Pool.create ~db ~name:"fairness_rates" in
  let w1 = Pool.register pool ~name:"worker-1" ~capacity:1 |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" ~capacity:1 |> Result.get_ok in
  let alice = Pool.client pool ~client_id:"alice" in
  let bob = Pool.client pool ~client_id:"bob" in
  Pool.Client.set_rate alice 5.0;
  Pool.Client.set_rate bob 1.0;
  Pool.set_active w1 true;
  Pool.set_active w2 true;
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
  Alcotest.(check pop_result) "Worker 1 / job 1" (Ok "a1") (job_state w1a);
  Alcotest.(check pop_result) "Worker 2 / job 1" (Ok "a2") (job_state w2a);
  Alcotest.(check string) "Bob's jobs aren't all last" "\
    capacity: 2\n\
    queue: (backlog) [bob:b3@20s bob:b2@10s alice:a3@4s bob:b1@0s]\n\
    registered:\n\
    \  worker-1 (0): []\n\
    \  worker-2 (0): []\n\
    clients: alice(5)+6s bob(1)+30s\n\
    cached: : [worker-1; worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Pool.release w2;
  flush_queue w1 ~expect:["b1"; "a3"; "b2"; "b3"]

let test_case name fn =
  Alcotest_lwt.test_case name `Quick @@ fun _ () ->
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

let suite = [
  test_case "scheduling" simple;
  test_case "cached_scheduling" cached_scheduling;
  test_case "unbalanced" unbalanced;
  test_case "no_workers" no_workers;
  test_case "persist" persist;
  test_case "urgent" urgent;
  test_case "urgent_worker" urgent_worker;
  test_case "inactive" inactive;
  test_case "cancel_worker_queue" cancel_worker_queue;
  test_case "push_back" push_back;
  test_case "fairness" fairness;
  test_case "fairness_rates" fairness_rates;
]

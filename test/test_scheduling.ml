open Lwt.Infix

module Item = struct
  type t = {
    job : string;
    cache_hint : string;
  }

  type cache_hint = string

  let cache_hint t = t.cache_hint

  let cost_estimate _ = Build_scheduler.S.{ cached = 1; non_cached = 5 }

  let pp f t = Fmt.string f t.job
end

let job ?(cache_hint="") job = { Item.job; cache_hint }

module Pool = Build_scheduler.Pool.Make(Item)

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

(* Assign three jobs to two workers. *)
let simple () =
  let pool = Pool.create ~name:"simple" in
  let w1 = Pool.register pool ~name:"worker-1" |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" |> Result.get_ok in
  let w1a = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  Pool.submit pool @@ job "job1";
  Pool.submit pool @@ job "job2";
  Pool.submit pool @@ job "job3";
  Lwt.pause () >>= fun () ->
  Alcotest.(check pop_result) "Worker 1 / job 1" (Ok "job1") (job_state w1a);
  Alcotest.(check pop_result) "Worker 2 / job 1" (Ok "job2") (job_state w2a);
  Pool.release w2;
  flush_queue w1 ~expect:["job3"]

(* Bias assignment towards workers that have things cached. *)
let cached_scheduling () =
  let pool = Pool.create ~name:"cached_scheduling" in
  let w1 = Pool.register pool ~name:"worker-1" |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" |> Result.get_ok in
  let w1a = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  Pool.submit pool @@ job "job1" ~cache_hint:"a";
  Pool.submit pool @@ job "job2" ~cache_hint:"b";
  Pool.submit pool @@ job "job3" ~cache_hint:"a";
  Pool.submit pool @@ job "job4" ~cache_hint:"a";
  Pool.submit pool @@ job "job5" ~cache_hint:"c";
  Alcotest.(check string) "Jobs queued" "\
    queue: (backlog) [job5 job4 job3]\n\
    registered:\n\
    \  worker-1 (5): [job1(5)]\n\
    \  worker-2 (5): [job2(5)]\n\
    cached: \n\
    will_cache: a: [worker-1], b: [worker-2]\n" (Fmt.to_to_string Pool.dump pool);
  Lwt.pause () >>= fun () ->
  Alcotest.(check string) "Jobs started" "\
    queue: (backlog) [job5 job4 job3]\n\
    registered:\n\
    \  worker-1 (0): []\n\
    \  worker-2 (0): []\n\
    cached: a: [worker-1], b: [worker-2]\n\
    will_cache: \n" (Fmt.to_to_string Pool.dump pool);
  Alcotest.(check pop_result) "Worker 1 / job 1" (Ok "job1") (job_state w1a);
  Alcotest.(check pop_result) "Worker 2 / job 1" (Ok "job2") (job_state w2a);
  (* Worker 2 asks for another job, but the next two jobs would be better done on worker-1. *)
  let w2b = Pool.pop w2 in
  Alcotest.(check string) "Jobs 3 and 4 assigned to worker-1" "\
    queue: (backlog) []\n\
    registered:\n\
    \  worker-1 (2): [job4(1) job3(1)]\n\
    \  worker-2 (0): []\n\
    cached: a: [worker-1], b: [worker-2], c: [worker-2]\n\
    will_cache: \n" (Fmt.to_to_string Pool.dump pool);
  Alcotest.(check pop_result) "Worker 2 / job 2" (Ok "job5") (job_state w2b);
  (* Worker 1 leaves. Its two queued jobs get reassigned. *)
  let w2c = Pool.pop w2 in
  Alcotest.(check pop_result) "Worker 2 / job 3" (Error "pending") (job_state w2c);
  Logs.info (fun f -> f "@[<v2>w1 about to leave:@,%a@]" Pool.dump pool);
  Pool.release w1;
  Lwt.pause () >>= fun () ->
  Alcotest.(check string) "Worker-1's jobs reassigned" "\
    queue: (backlog) [job4]\n\
    registered:\n\
    \  worker-2 (0): []\n\
    cached: a: [worker-1; worker-2], b: [worker-2], c: [worker-2]\n\
    will_cache: \n" (Fmt.to_to_string Pool.dump pool);
  Alcotest.(check pop_result) "Worker 2 / job 3" (Ok "job3") (job_state w2c);
  let w2d = Pool.pop w2 in
  Alcotest.(check pop_result) "Worker 2 / job 4" (Ok "job4") (job_state w2d);
  Pool.release w2;
  Alcotest.(check string) "Idle" "\
    queue: (backlog) []\n\
    registered:\n\
    cached: a: [worker-1; worker-2], b: [worker-2], c: [worker-2]\n\
    will_cache: \n" (Fmt.to_to_string Pool.dump pool);
  Lwt.return_unit

(* Bias assignment towards workers that have things cached, but not so much that it takes longer. *)
let unbalanced () =
  let pool = Pool.create ~name:"unbalanced" in
  let w1 = Pool.register pool ~name:"worker-1" |> Result.get_ok in
  let w2 = Pool.register pool ~name:"worker-2" |> Result.get_ok in
  Pool.submit pool @@ job "job1" ~cache_hint:"a";
  Pool.submit pool @@ job "job2" ~cache_hint:"a";
  Pool.submit pool @@ job "job3" ~cache_hint:"a";
  Pool.submit pool @@ job "job4" ~cache_hint:"a";
  Pool.submit pool @@ job "job5" ~cache_hint:"a";
  Pool.submit pool @@ job "job6" ~cache_hint:"a";
  Pool.submit pool @@ job "job7" ~cache_hint:"a";
  Pool.submit pool @@ job "job8" ~cache_hint:"a";
  let _ = Pool.pop w1 in
  let w2a = Pool.pop w2 in
  Lwt.pause () >>= fun () ->
  Alcotest.(check string) "Worker-2 got jobs eventually" "\
    queue: (backlog) []\n\
    registered:\n\
    \  worker-1 (6): [job7(1) job6(1) job5(1) job4(1) job3(1) job2(1)]\n\
    \  worker-2 (0): []\n\
    cached: a: [worker-1; worker-2]\n\
    will_cache: \n" (Fmt.to_to_string Pool.dump pool);
  Alcotest.(check pop_result) "Worker 2 / job 1" (Ok "job8") (job_state w2a);
  Pool.release w1;
  flush_queue w2 ~expect:["job2"; "job3"; "job4"; "job5"; "job6"; "job7"]

(* There are no workers available sometimes. *)
let no_workers () =
  let pool = Pool.create ~name:"no_workers" in
  Pool.submit pool @@ job "job1" ~cache_hint:"a";
  Pool.submit pool @@ job "job2" ~cache_hint:"a";
  let w1 = Pool.register pool ~name:"worker-1" |> Result.get_ok in
  let _ = Pool.pop w1 in
  Pool.release w1;
  Lwt.pause () >>= fun () ->
  Alcotest.(check string) "Worker-1 gone" "\
    queue: (backlog) [job2]\n\
    registered:\n\
    cached: a: [worker-1]\n\
    will_cache: \n" (Fmt.to_to_string Pool.dump pool);
  let w1 = Pool.register pool ~name:"worker-1" |> Result.get_ok in
  flush_queue w1 ~expect:["job2"]

let test_case name fn =
  Alcotest_lwt.test_case name `Quick @@ fun _ () ->
  fn () >|= fun () ->
  Prometheus.CollectorRegistry.(collect default)
  |> Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output
  |> String.split_on_char '\n'
  |> List.iter (fun line ->
      if Astring.String.is_prefix ~affix:"scheduler_pool_" line then
        match Astring.String.cut ~sep:" " line with
        | None -> Fmt.failwith "Bad metrics line: %S" line
        | Some (key, value) ->
          if float_of_string value <> 0.0 then
            Fmt.failwith "Non-zero metric after test: %s=%s" key value
    )

let suite = [
  test_case "scheduling" simple;
  test_case "cached_scheduling" cached_scheduling;
  test_case "unbalanced" unbalanced;
  test_case "no_workers" no_workers;
]

open Lwt.Syntax

let default_sleep_duration n' =
  let base_sleep_time = 2.0 in
  let n = Int.to_float n' in
  n *. base_sleep_time *. Float.pow 2.0 n

type ('retry, 'fatal) error =
  [ `Retry of 'retry
  | `Fatal of 'fatal
  ]

let pp_opaque fmt _ = Format.fprintf fmt "<opaque>"

let pp_error ?(retry = pp_opaque) ?(fatal = pp_opaque) fmt err =
  match err with
  | `Retry r -> Format.fprintf fmt "`Retry %a" retry r
  | `Fatal f -> Format.fprintf fmt "`Fatal %a" fatal f

let equal_error ~retry ~fatal a b =
  match a, b with
  | `Retry a', `Retry b' -> retry a' b'
  | `Fatal a', `Fatal b' -> fatal a' b'
  | _ -> false

type ('ok, 'retry, 'fatal) attempt = ('ok, ('retry, 'fatal) error) result

let is_retryable = function
  | Error (`Retry _) -> true
  | _ -> false

let on_error
    (f : unit -> ('ok, 'retry, 'fatal) attempt Lwt.t)
  : ('ok, 'retry, 'fatal) attempt Lwt_stream.t
  =
  let stop = ref false in
  let attempt () =
    if !stop then
      Lwt.return_none
    else
      let+ result = f () in
      stop := not (is_retryable result);
      Some result
  in
  Lwt_stream.from attempt

let numbered attempts : (int * _) Lwt_stream.t =
  let i = ref 0 in
  let indexes = Lwt_stream.from_direct (fun () -> let n = !i in incr i; Some n) in
  Lwt_stream.combine indexes attempts

let with_sleep ?(duration=default_sleep_duration) attempts =
  attempts
  |> numbered
  |> Lwt_stream.map_s (fun (attempt_number, attempt_result) ->
      let+ () = Lwt_unix.sleep @@ duration attempt_number in
      attempt_result)

let n_times n attempts =
  (* The first attempt is a try, and REtries start counting from n + 1 *)
  let retries = n + 1 in
  let+ attempts = Lwt_stream.nget retries attempts in
  match List.rev attempts with
  | last :: _ -> last
  | _ -> failwith "impossible"

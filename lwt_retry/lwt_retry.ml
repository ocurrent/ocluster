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

type ('ok, 'retry, 'fatal) attempt = ('ok, ('retry, 'fatal) error * int) result

let on_error
    (f : unit -> ('ok, ('retry, 'fatal) error) result Lwt.t)
  : ('ok, 'retry, 'fatal) attempt Lwt_stream.t
  =
  let i = ref 0 in
  let stop = ref false in
  Lwt_stream.from begin fun () ->
    incr i;
    let+ result = f () in
    if !stop then None else
      match result with
      | Error (`Retry _ as retry) -> Some (Error (retry, !i))
      | Error (`Fatal _ as fatal) -> stop := true; Some (Error (fatal, !i))
      | Ok _ as ok -> stop := true; Some ok
  end

let map_retry
    (f : 'retry -> int -> 'a)
    (attempts : (_, 'retry, _) attempt Lwt_stream.t)
  : (_, 'a, _) attempt Lwt_stream.t =
  attempts
  |> Lwt_stream.map begin Result.map_error (function
      | (`Retry r, n) -> `Retry (f r n), n
      | (`Fatal _, _) as fatal -> fatal)
  end

let with_sleep ?(duration=default_sleep_duration) (attempts : _ attempt Lwt_stream.t) : _ attempt Lwt_stream.t =
  attempts
  |> Lwt_stream.map_s begin function
    | Ok ok -> Lwt.return_ok ok
    | Error (err, n) ->
      let* () = Lwt_unix.sleep @@ duration n in
      Lwt.return_error (err, n)
  end

let n_times n attempts =
  (* The first attempt is a try, and REtries start counting from n + 1 *)
  let retries = n + 1 in
  let+ attempts = Lwt_stream.nget retries attempts in
  match List.rev attempts with
  | last :: _ -> last
  | _ -> failwith "impossible"

(** Utilities for retrying Lwt computations *)

(* NOTE: We expect this library to be useful in other parts of the ocurrent
   stack, but this is where we need it first. Please promote this to more  a
   more general, shared location when needed, instead of copying the code. *)

type ('retry, 'fatal) error =
  [ `Retry of 'retry
  | `Fatal of 'fatal
  ]
(** The type of errors that a retryable computation can produce.

    - [`Retry r] when [r] represents an error that can be retried.
    - [`Fatal f] when [f] represents an error that cannot be retried. *)

type ('ok, 'retry, 'fatal) attempt = ('ok, ('retry, 'fatal) error) result
(** A [('ok, 'retry, 'fatal) attempt] is an alias for the [result] of a
    retryable computation.

    - [Ok v] produces a successful value [v]
    - [Error err] produces the {!type:error} [err] *)

val pp_error :
  ?retry:(Format.formatter -> 'retry -> unit) ->
  ?fatal:(Format.formatter -> 'fatal -> unit) ->
  Format.formatter -> ('retry, 'fatal) error -> unit
(** [pp_error ~retry ~fatal] is a formatter for {!type:error}s that formats
    fatal and retryable errors according to the provided formatters.

    If either formatter is not provided, a default formatter will represent the
    values as ["<opaque>"]. *)

val equal_error :
  retry:('retry -> 'retry -> bool) ->
  fatal:('fatal -> 'fatal -> bool) ->
  ('retry, 'fatal) error ->
  ('retry, 'fatal) error ->
  bool

val on_error :
  (unit -> ('ok, 'retry, 'fatal) attempt Lwt.t) ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t
(** [on_error f] is a stream of attempts to compute [f]. The stream will continue until
    the computation succeeds or produces a fatal error.

    Examples

    {[
      # let success () = Lwt.return_ok ();;
      val success : unit -> (unit, 'a) result Lwt.t = <fun>
      # Lwt_retry.(success |> on_error) |> Lwt_stream.to_list;;
      - : (unit, 'a, 'b) Lwt_retry.attempt list = [Ok ()]

      # let fatal_failure () = Lwt.return_error (`Fatal ());;
      val fatal_failure : unit -> ('a, [> `Fatal of unit ]) result Lwt.t = <fun>
      # Lwt_retry.(fatal_failure |> on_error) |> Lwt_stream.to_list;;
      - : ('a, 'b, unit) Lwt_retry.attempt list = [Error (`Fatal ())]

      # let retryable_error () = Lwt.return_error (`Retry ());;
      val retryable_error : unit -> ('a, [> `Retry of unit ]) result Lwt.t = <fun>
      # Lwt_retry.(retryable_error |> on_error) |> Lwt_stream.nget 3;;
      - : ('a, unit, 'b) Lwt_retry.attempt list =
      [Error (`Retry ()); Error (`Retry ()); Error (`Retry ())]
    ]}*)

val with_sleep :
  ?duration:(int -> float) ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t
(** [with_sleep ~duration attempts] is the stream of [attempts] with a sleep
    added after computing each [n]th retryable attempt based on [duration n].

    @param duration the optional sleep duration. This defaults to an exponential
    backoff computed as n * 2 * (1.5 ^ n), which gives the approximate sequence
    0s -> 3s -> 9s -> 20.25 -> 40.5s -> 75.9s -> 136.7 -> ...

    Examples
    {[
      # let f () = Lwt.return_error (`Retry ());;
      # let attempts_with_sleeps = Lwt_retry.(f |> on_error |> with_sleep);;

      # Lwt_stream.get attempts_with_sleeps;;
      (* computed immediately *)
      Some (Error (`Retry ()))

      # Lwt_stream.get attempts_with_sleeps;;
      (* computed after 3 seconds *)
      Some (Error (`Retry ()))

      # Lwt_stream.get attempts_with_sleeps;;
      (* computed after 9 seconds *)
      Some (Error (`Retry ()))

      (* a stream with a constant 1s sleep between attempts *)
      # let attempts_with_constant_sleeps =
          Lwt_retry.(f |> on_error |> with_sleep ~duration:(fun _ -> 1.0));;
    ]} *)

val n_times :
  int ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t ->
  ('ok, ('retry, 'fatal) error) result Lwt.t
(** [n_times n attempts] is [Ok v] if one of the [attempts] succeeds within [n]
    retries (or [n+1] attempts), [Error (`Fatal f)] if any of the attempts
    results in the fatal error, or [Error (`Retry r)] if all [n] retries are
    exhausted, with the [n+1]th attempt resulting in the retry error.

    In particular [n_times 0 attempts] will *try* 1 attempt but *retry* 0, so it
    is guaranteed to produce some result.

    Examples

    {[
      # let f () =
          let i = ref 0 in
          fun () -> Lwt.return_error (if !i < 3 then (incr i; `Retry !i) else `Fatal "error!");;
      # Lwt_retry.(f () |> on_error |> n_times 0);;
      Error (`Retry 1)
      # Lwt_retry.(f () |> on_error |> n_times 4);;
      Error (`Fatal "error!")
    ]} *)

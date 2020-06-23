open Lwt.Infix

let max_chunk_size = 10240L

type t = {
  data : Buffer.t;
  mutable cond : [ `Running of unit Lwt_condition.t
                 | `Finished ]
}

let create () = 
  {
    data = Buffer.create 10240;
    cond = `Running (Lwt_condition.create ());
  }

let rec stream t ~start =
  let len = Int64.of_int (Buffer.length t.data) in
  let start = if start < 0L then max 0L (Int64.add len start) else start in
  let avail = Int64.sub len start in
  if avail < 0L then Fmt.failwith "Start value out of range!";
  if avail = 0L then (
    match t.cond with
    | `Running cond ->
      Lwt_condition.wait cond >>= fun () ->
      stream t ~start
    | `Finished ->
      Lwt.return ("", start)
  ) else (
    let chunk = min avail max_chunk_size in
    let next = Int64.add start chunk in
    let start = Int64.to_int start in
    let avail = Int64.to_int avail in
    Lwt.return (Buffer.sub t.data start avail, next)
  )

let write t data =
  match t.cond with
  | `Running cond ->
    Buffer.add_string t.data data;
    Lwt_condition.broadcast cond ()
  | `Finished ->
    Fmt.failwith "Attempt to write to log after close: %S" data

let copy_from_stream t src =
  let rec aux () =
    Lwt_io.read ~count:4096 src >>= function
    | "" -> Lwt.return_unit
    | data -> write t data; aux ()
  in
  aux ()

let close t =
  match t.cond with
  | `Running cond ->
    t.cond <- `Finished;
    Lwt_condition.broadcast cond ()
  | `Finished ->
    Fmt.failwith "Log already closed!"

let info t fmt =
  Fmt.kstrf (write t) (fmt ^^ "@.")

type error = [
  | `Cancelled
  | `Exit_code of int
  | `Msg of string
]

val exec :
  log:Log_data.t ->
  switch:Lwt_switch.t ->
  ?stdin:string ->
  ?stderr:Lwt_process.redirection ->
  string list ->
  (unit, [> error]) Lwt_result.t

type error = [
  | `Cancelled
  | `Exit_code of int
  | `Msg of string
]

val exec :
  label:string ->
  log:Log_data.t ->
  switch:Lwt_switch.t ->
  ?env:string array ->
  ?stdin:string ->
  ?stderr:Lwt_process.redirection ->
  string list ->
  (unit, [> error]) Lwt_result.t

val check_call :
  label:string ->
  log:Log_data.t ->
  switch:Lwt_switch.t ->
  ?env:string array ->
  ?stdin:string ->
  ?stderr:Lwt_process.redirection ->
  string list ->
  (unit, [> `Cancelled | `Msg of string]) Lwt_result.t

val check_output :
  label:string ->
  log:Log_data.t ->
  switch:Lwt_switch.t ->
  ?env:string array ->
  ?stdin:string ->
  string list ->
  (string, [> `Cancelled | `Msg of string]) Lwt_result.t

type t

val create : Obuilder.Store_spec.t -> t Lwt.t

val build : t ->
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  spec:Obuilder.Spec.stage ->
  src_dir:string -> (string, [ `Cancelled | `Msg of string ]) Lwt_result.t

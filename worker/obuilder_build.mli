type t

val create : [< `Btrfs of string | `Zfs of string ] -> t Lwt.t

val build : t ->
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  spec:Obuilder.Spec.stage ->
  src_dir:string -> (string, [ `Cancelled | `Msg of string ]) Lwt_result.t

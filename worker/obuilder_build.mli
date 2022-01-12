type t

module Config : sig
  type t

  val v : Obuilder.Runc_sandbox.config -> [ `Btrfs of string | `Rsync of string | `Zfs of string ] -> t
end

val create : ?prune_threshold:float -> Config.t -> t Lwt.t

val build : t ->
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  spec:Obuilder.Spec.t ->
  src_dir:string ->
  secrets:(string * string) list -> (string, [ `Cancelled | `Msg of string ]) Lwt_result.t

val healthcheck : t -> (unit, [> `Msg of string]) Lwt_result.t

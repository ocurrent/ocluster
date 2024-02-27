type t

module Config : sig
  type t

  val v : [ `Native of Obuilder.Native_sandbox.config
          | `Docker of Obuilder.Docker_sandbox.config ]
          -> Obuilder.Store_spec.store Lwt.t -> t
end

val create : ?prune_threshold:float -> ?prune_limit:int -> Config.t -> t Lwt.t

val build : t ->
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  spec:Obuilder.Spec.t ->
  src_dir:string ->
  secrets:(string * string) list -> (string, [ `Cancelled | `Msg of string ]) Lwt_result.t

val healthcheck : t -> (unit, [> `Msg of string]) Lwt_result.t

val cache_stats : t -> int * int

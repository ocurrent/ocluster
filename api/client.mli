open Capnp_rpc_lwt

module Logger : sig
  type t = Raw.Client.Logger.t Capability.t
  
  val log : msg:string -> t -> (unit, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
end

module Agent : sig
  type t = Raw.Client.Agent.t Capability.t

  val exec : cmd:string -> t -> (int32, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
end

module Cluster : sig
  type t = Raw.Client.Cluster.t Capability.t

  val register : hostname:string -> callback:Agent.t -> t -> Logger.t
end
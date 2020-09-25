type job_spec = [ 
  | `Docker of [ `Contents of string | `Path of string ] * Cluster_api.Docker.Spec.options
  | `Obuilder of [ `Contents of string ]
]

val run :
  ?switch:Lwt_switch.t ->
  ?build:(switch:Lwt_switch.t ->
          log:Log_data.t ->
          src:string ->
          job_spec ->
          (string, [`Cancelled | `Msg of string]) Lwt_result.t) ->
  ?allow_push:string list ->
  ?prune_threshold:float ->
  ?obuilder:[ `Zfs of string | `Btrfs of string ] ->
  capacity:int ->
  name:string ->
  Cluster_api.Raw.Client.Registration.t Capnp_rpc_lwt.Sturdy_ref.t ->
  unit Lwt.t
(** [run ~capacity ~name registry] runs a builder that connects to registry and runs up to [capacity] jobs at once.
    The builder registers using the unique ID [name].
    @param switch Turning this off causes the builder to exit (for unit-tests)
    @param build Used to override the default build action (for unit-tests)
    @param allow_push Docker repositories to which results can be pushed
    @param prune_threshold Stop and run "docker system prune -af" if free-space is less than this percentage (0 to 100). *)

module Process = Process
module Log_data = Log_data

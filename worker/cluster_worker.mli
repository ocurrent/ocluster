type job_spec = [
  | `Docker of [ `Contents of string | `Path of string ] * Cluster_api.Docker.Spec.options
  | `Obuilder of [ `Contents of string ]
  | `Custom of Cluster_api.Custom.recv
]

module Obuilder_config : sig
  type t

  val v : Obuilder.Sandbox.config -> Obuilder.Store_spec.t -> t
end

type build =
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  src:string ->
  secrets:(string * string) list ->
  job_spec ->
  (string, [`Cancelled | `Msg of string]) Lwt_result.t

val default_build : ?obuilder:Obuilder_build.t -> build
(** The default build that is used if no [build] argument is given to {! run}. *)

val run :
  ?switch:Lwt_switch.t ->
  ?build:build ->
  ?allow_push:string list ->
  healthcheck_period:float ->
  ?prune_threshold:float ->
  ?docker_max_df_size:float ->
  ?obuilder_prune_threshold:float ->
  ?obuilder:Obuilder_config.t ->
  ?additional_metrics:(string * Uri.t) list ->
  update:(unit -> (unit -> unit Lwt.t) Lwt.t) ->
  capacity:int ->
  name:string ->
  state_dir:string ->
  Cluster_api.Raw.Client.Registration.t Capnp_rpc_lwt.Sturdy_ref.t ->
  unit Lwt.t
(** [run ~capacity ~name ~state_dir registry] runs a builder that connects to registry and runs up
    to [capacity] jobs at once. The builder registers using the unique ID [name].
    @param switch Turning this off causes the builder to exit (for unit-tests).
    @param build Used to override the default build action (for unit-tests or custom job
      specifications).
    @param allow_push Docker repositories to which results can be pushed.
    @param healthcheck_period Time period, in seconds, at which the health of the worker is checked.
    @param prune_threshold Stop and run [docker system prune -af] if free-space is less than this
      percentage (0 to 100).
    @param docker_max_df_size Run [docker system df] to get the amount of memory being taken up by
      the images and if this is greater than [docker_max_df_size], run [docker system prune].
    @param obuilder_prune_threshold The threshold for OBuilder to prune the store of cached build
      steps.
    @param obuilder OBuilder's configuration.
    @param additional_metrics A list of additional prometheus endpoints with a descriptive name to
      collect.
    @param update Function to run on "selfUpdate" requests. It should do any preparation (such as
      downloading new images), then return a function to do the actual update. This is so that the
      first part can run while the node finishes its remaining jobs. The second part is called once
      all jobs are finished.  If the second function returns, the process will exit.
    @param capacity The number of builds that can run in parallel.
    @param name Unique builder name.
    @param state_dir A persistent directory for Git caches, etc. *)

module Process = Process
module Log_data = Log_data

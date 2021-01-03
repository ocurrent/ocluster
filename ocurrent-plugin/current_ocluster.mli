(** This module provides an OCurrent plugin for building with OCluster. *)

module Connection = Connection

type t
(** The configuration for accessing the build cluster. *)

type urgency = [
  | `Auto       (** Builds will be marked as urgent whenever there isn't some
                    existing latched output that can be used while waiting. *)
  | `Always     (** All builds will be marked as urgent. *)
  | `Never      (** No builds will be urgent. *)
]

val v :
  ?timeout:Duration.t ->
  ?push_auth:(string * string) ->
  ?urgent:urgency ->
  Connection.t ->
  t
(** [v conn] is a builder that submits jobs using [conn].
    @param push_auth : the username and password to use when pushing to the Docker Hub staging area.
    @param timeout : default timeout
    @param urgent : when to mark builds as urgent (default [`Auto]). *)

val with_timeout : Duration.t option -> t -> t
(** [with_timeout x t] is a copy of [t] with the specified timeout, but still
    sharing the same connection. *)

val with_push_auth : (string * string) option -> t -> t
(** [with_push_auth x t] is a copy of [t] with the specified push settings, but still
    sharing the same connection. *)

val with_urgent : urgency -> t -> t
(** [with_urgent x t] is a copy of [t] with urgency policy [x]. *)

val build : 
  ?cache_hint:string ->
  t ->
  pool:string ->
  src:Current_git.Commit_id.t list Current.t ->
  options:Cluster_api.Docker.Spec.options ->
  [ `Contents of string Current.t | `Path of string ] ->
  unit Current.t
(** [build t ~pool ~src ~options dockerfile] builds [dockerfile] in context [src] using pool [pool] within build cluster [t].
    Note: all commits in [src] must be in the same repository. *)

val build_and_push :
  ?cache_hint:string ->
  t ->
  push_target:Cluster_api.Docker.Image_id.t ->
  pool:string ->
  src:Current_git.Commit_id.t list Current.t ->
  options:Cluster_api.Docker.Spec.options ->
  [ `Contents of string Current.t | `Path of string ] ->
  string Current.t
(** [build_and_push] is like [build] but also uploads the resulting image to [push_target] on success.
    Returns the RepoId of the pushed image.
    If [t] doesn't have [push_auth] configured, this still tests the build, but returns an error at the end. *)

val build_obuilder :
  ?cache_hint:string ->
  t ->
  pool:string ->
  src:Current_git.Commit_id.t list Current.t ->
  Cluster_api.Obuilder_job.Spec.t Current.t ->
  unit Current.t
(** [build_obuilder t ~pool ~src spec] builds [spec] in context [src] using pool [pool] within build cluster [t].
    Note: all commits in [src] must be in the same repository. *)

val build_nix :
  ?cache_hint:string ->
  t ->
  pool:string ->
  Cluster_api.Nix_build.Spec.t Current.t ->
  string Current.t
(** [build_nix t ~pool spec] builds [spec] using pool [pool] within build cluster [t].
    Note: all dependencies of [spec] must be available in a binary cache,
    since only the leaf derivation is provided *)

module Raw : sig
  val build : 
    ?cache_hint:string ->
    t ->
    pool:string ->
    src:Current_git.Commit_id.t list ->
    options:Cluster_api.Docker.Spec.options ->
    [ `Contents of string | `Path of string ] ->
    unit Current.Primitive.t

  val build_and_push :
    ?cache_hint:string ->
    t ->
    push_target:Cluster_api.Docker.Image_id.t ->
    pool:string ->
    src:Current_git.Commit_id.t list ->
    options:Cluster_api.Docker.Spec.options ->
    [ `Contents of string | `Path of string ] ->
    string Current.Primitive.t

  val build_obuilder :
    ?cache_hint:string ->
    t ->
    pool:string ->
    src:Current_git.Commit_id.t list ->
    Cluster_api.Obuilder_job.Spec.t ->
    unit Current.Primitive.t

  val build_nix :
    ?cache_hint:string ->
    t ->
    pool:string ->
    Cluster_api.Nix_build.Spec.t ->
    string Current.Primitive.t
end

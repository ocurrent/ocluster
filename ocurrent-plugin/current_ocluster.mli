(** This module provides an OCurrent plugin for building with OCluster. *)

open Capnp_rpc_lwt

type t
(** The configuration for accessing the build cluster. *)

val v :
  ?timeout:Duration.t ->
  ?push_auth:(string * string) ->
  [ `Submission_f4e8a768b32a7c42 ] Sturdy_ref.t -> t
(** [v submission_service] is a builder that submits jobs to [submission_service].
    @param push_auth : the username and password to use when pushing to the Docker Hub staging area.
    @param timeout : default timeout *)

val with_timeout : Duration.t option -> t -> t
(** [with_timeout x t] is a copy of [t] with the specified timeout, but still
    sharing the same connection. *)

val with_push_auth : (string * string) option -> t -> t
(** [with_push_auth x t] is a copy of [t] with the specified push settings, but still
    sharing the same connection. *)

val build : 
  ?cache_hint:string ->
  t ->
  pool:string ->
  src:Current_git.Commit_id.t list Current.t ->
  options:Cluster_api.Docker.Spec.options ->
  [ `Contents of string Current.t | `Path of string ] ->
  unit Current.t
(** [build t ~pool ~src dockerfile] builds [dockerfile] in context [src] using pool [pool] within build cluster [t].
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
end

(** Fetch and cache the Git repositories used to create the build contexts.
    It works like this:

    1. We clone the repository in mirror mode. This means that we also see
       PRs. We could just pull the hashes we want, but future updates can
       be more efficient if we track what we have in branches.

    2. We use a non-bare repository because we want submodules cached too
       (using worktrees, the submodule goes into the worktree subdirectory and
       is lost when the worktree is deleted).

    3. We reset to the first commit and then merge the others. This makes it
       easy to e.g. test a PR merged with master. Then we fetch any submodules
       (we don't attempt to merge submodule changes).

    4. Finally, we move all the checked-out files to our desired temporary
       directory (on the same FS) and release the repository lock. *)

val with_build_context :
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  Cluster_api.Raw.Reader.JobDescr.t ->
  (string -> ('a, Process.error) Lwt_result.t) ->
  ('a, Process.error) Lwt_result.t
(** [with_build_context ~switch ~log descr fn] runs [fn dir], where [dir] is a
    temporary directory containing the requested build context. *)

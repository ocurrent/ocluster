open Lwt.Infix

module Hash = Digestif.SHA1

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

type t = {
  state_dir : string;
}

let repos_dir t = t.state_dir / "git"

let dir_exists d =
  match Unix.lstat d with
  | Unix.{ st_kind = S_DIR; _ } -> true
  | _ -> false
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> false

let ensure_dir path =
  if not (dir_exists path) then Unix.mkdir path 0o700

let get_tmp_dir t =
  t.state_dir / "tmp"

let git_merge_env =
  let orig = Unix.environment () |> Array.to_list in
  "GIT_AUTHOR_NAME=ocluster" ::
  "GIT_COMMITTER_NAME=ocluster" ::
  "EMAIL=ocluster@ocurrent.org" :: orig
  |> Array.of_list

module Repo = struct
  type nonrec t = {
    context : t;
    url : Uri.t;
    lock : Lwt_mutex.t;
  }

  let id t =
    let base = Filename.basename (Uri.path t.url) in
    let digest = Hash.digest_string (Uri.to_string t.url) in
    Fmt.strf "%s-%s" base (Hash.to_hex digest)

  let local_copy t =
    repos_dir t.context / id t

  let v context url =
    let url = Uri.of_string url in
    match Uri.scheme url with
    | Some "git" | Some "http" | Some "https" -> { context; url; lock = Lwt_mutex.create () }
    | Some x -> Fmt.failwith "Unsupported scheme %S in URL %a" x Uri.pp url
    | None -> Fmt.failwith "Missing scheme in URL %a" Uri.pp url

  let has_commits t cs =
    let local_repo = local_copy t in
    if dir_exists local_repo then (
      let rec aux = function
        | [] -> Lwt_result.return true
        | c :: cs ->
          Lwt_process.exec ~cwd:local_repo ("", [| "git"; "cat-file"; "-e"; Hash.to_hex c |]) >>= function
          | Unix.WEXITED 0 -> aux cs
          | Unix.WEXITED _ -> Lwt_result.return false
          | _ -> Fmt.failwith "git cat-file crashed!"
      in
      aux cs
    ) else Lwt_result.return false      (* Don't let ocaml-git try to init a new repository! *)

  let fetch ~switch ~log t =
    let local_repo = local_copy t in
    begin
      if dir_exists local_repo then Lwt_result.return ()
      else (
        Process.check_call ~label:"git-init" ~switch ~log ["git"; "init"; local_repo] >>!= fun () ->
        let config k v = Process.check_call ~label:"git-config" ~switch ~log ["git"; "-C"; local_repo; "config"; "--add"; k; v] in
        config "remote.origin.url" (Uri.to_string t.url) >>!= fun () ->
        config "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*" >>!= fun () ->
        config "remote.origin.fetch" "+refs/pull/*:refs/remotes/pull/*"
      )
    end >>!= fun () ->
    (* This reset might avoid `fatal: cannot chdir to '../../../ocurrent': No such file or directory` errors *)
    Process.check_call ~label:"git-reset" ~switch ~log ["git"; "-C"; local_repo; "reset"; "--hard"] >>!= fun () ->
    Process.check_call ~label:"git-submodule-deinit" ~switch ~log ["git"; "-C"; local_repo; "submodule"; "deinit"; "--all"; "-f"] >>!= fun () ->
    Process.check_call ~label:"git-fetch" ~switch ~log
      ["git"; "-C"; local_repo; "fetch"; "-q"; "--update-head-ok"; "--recurse-submodules=no"; "origin"]
end

let repos = Hashtbl.create 1000

let repo t url =
  match Hashtbl.find_opt repos url with
  | Some x -> x
  | None ->
    let repo = Repo.v t url in
    Hashtbl.add repos url repo;
    repo

let rec lwt_result_list_iter_s f = function
  | [] -> Lwt_result.return ()
  | x :: xs ->
    f x >>!= fun () ->
    lwt_result_list_iter_s f xs

let include_git descr =
  match Cluster_api.Submission.get_action descr with
  | Docker_build db -> db.options.include_git
  | Obuilder_build _ | Nix_build _ -> false

let build_context t ~log ~tmpdir descr =
  match Cluster_api.Raw.Reader.JobDescr.commits_get_list descr |> List.map Hash.of_hex with
  | [] ->
    Lwt_result.return ()
  | (c :: cs) as commits ->
    let repository = repo t (Cluster_api.Raw.Reader.JobDescr.repository_get descr) in
    Lwt_mutex.with_lock repository.lock (fun () ->
        Lwt_switch.with_switch @@ fun switch -> (* Don't let the user cancel these operations. *)
        begin
          Repo.has_commits repository commits >>!= function
          | true -> Log_data.info log "All commits already cached"; Lwt_result.return ()
          | false -> Repo.fetch ~switch ~log repository
        end >>!= fun () ->
        let clone = Repo.local_copy repository in
        Process.check_call ~label:"git-reset" ~switch ~log ["git"; "-C"; clone; "reset"; "--hard"; Hash.to_hex c] >>!= fun () ->
        Process.check_call ~label:"git-submodule-deinit" ~switch ~log ["git"; "-C"; clone; "submodule"; "deinit"; "--all"; "-f"] >>!= fun () ->
        Process.check_call ~label:"git-clean" ~switch ~log ["git"; "-C"; clone; "clean"; "-fdx"] >>!= fun () ->
        let merge c = Process.check_call ~label:"git-merge" ~switch ~log ~env:git_merge_env ["git"; "-C"; clone; "merge"; Hash.to_hex c] in
        cs |> lwt_result_list_iter_s merge >>!= fun () ->
        Process.check_call ~label:"git-submodule-update" ~switch ~log ["git"; "-C"; clone; "submodule"; "update"; "--init"; "--recursive"] >>!= fun () ->
        Sys.readdir clone |> Array.iter (function
            | ".git" -> ()
            | name -> Unix.rename (clone / name) (tmpdir / name)
          );
        if include_git descr then (
          Process.check_call ~label:"cp .git" ~switch ~log ["cp"; "-a"; clone / ".git"; tmpdir / ".git"] >>!= fun () ->
          Process.check_call ~label:"git-config" ~switch ~log ["git"; "-C"; tmpdir; "config"; "--unset"; "remote.origin.fetch"; "/pull/"] >>!= fun () ->
          Lwt_result.return ()
        ) else (
          Lwt_result.return ()
        )
      )

let with_build_context t ~log descr fn =
  let tmp = get_tmp_dir t in
  Lwt_io.with_temp_dir ~parent:tmp ~prefix:"build-context-" @@ fun tmpdir ->
  build_context t ~log ~tmpdir descr >>!= fun () ->
  fn tmpdir

let v ~state_dir =
  ensure_dir state_dir;
  let t = { state_dir } in
  ensure_dir (get_tmp_dir t);
  t

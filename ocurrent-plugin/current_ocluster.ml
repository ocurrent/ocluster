open Lwt.Infix
open Capnp_rpc_lwt

let src = Logs.Src.create "current_ocluster" ~doc:"OCurrent OCluster plugin"
module Log = (val Logs.src_log src : Logs.LOG)

module Git = Current_git

let ( >>!= ) = Lwt_result.bind

(* This is shared by all jobs. *)
type connection = {
  sr : [`Submission_f4e8a768b32a7c42] Sturdy_ref.t;
  mutable sched : Cluster_api.Submission.t Lwt.t;
}

type urgency = [ `Auto | `Always | `Never ]

(* This may be copied and modified per job. *)
type t = {
  connection : connection;
  timeout : Duration.t option;
  push_auth : (string * string) option; (* Username/password for pushes *)
  cache_hint : string option;
  urgent : urgency;
}

let with_push_auth push_auth t = { t with push_auth }
let with_timeout timeout t = { t with timeout }
let with_urgent urgent t = { t with urgent }

let tail ~job build_job =
  let rec aux start =
    Cluster_api.Job.log build_job start >>= function
    | Error (`Capnp e) -> Lwt.return @@ Fmt.error_msg "%a" Capnp_rpc.Error.pp e
    | Ok ("", _) -> Lwt_result.return ()
    | Ok (data, next) ->
      Current.Job.write job data;
      aux next
  in aux 0L

module Op = struct
  let id = "ocluster-build"

  type nonrec t = t

  module Key = struct
    type commit_id = Git.Commit_id.t
    let commit_id_to_yojson x = `String (Git.Commit_id.hash x)

    type target = Cluster_api.Docker.Image_id.t
    let target_to_yojson x = `String (Cluster_api.Docker.Image_id.to_string x)

    type t = {
      dockerfile : [ `Contents of string | `Path of string ];
      src : commit_id list;
      options : Cluster_api.Docker.Spec.options;
      pool : string;
      push_target : target option;
    } [@@deriving to_yojson]

    let digest t = Yojson.Safe.to_string (to_yojson t)
  end

  module Value = Current.String

  (* Return a proxy to the scheduler, starting a new connection if we don't
     currently have a working one. *)
  let sched ~job t =
    let conn = t.connection in
    match Lwt.state conn.sched with
    | Lwt.Return cap when Capability.problem cap = None -> Lwt.return cap
    | Lwt.Sleep ->
      Current.Job.log job "Connecting to build cluster...";
      conn.sched      (* Already connecting; join that effort *)
    | _ ->
      Current.Job.log job "Connecting to build cluster...";
      let rec aux () =
        Lwt.catch
          (fun () ->
             Sturdy_ref.connect_exn conn.sr >>= fun cap ->
             Capability.wait_until_settled cap >|= fun () ->
             cap
          )
          (fun ex ->
             Log.warn (fun f -> f "Error connecting to build cluster (will retry): %a" Fmt.exn ex);
             Lwt_unix.sleep 10.0 >>= fun () ->
             aux ()
          )
      in
      conn.sched <- aux ();
      conn.sched

  (* This is called by [Current.Job] once the confirmation threshold allows the job to be submitted. *)
  let submit ~job ~pool ~action ~cache_hint ?src t ~priority ~switch =
    let ticket_ref = ref None in
    let cancel () =
      match !ticket_ref with
      | None -> Lwt.return_unit
      | Some ticket ->
        Cluster_api.Ticket.cancel ticket >|= function
        | Ok () -> ()
        | Error (`Capnp e) -> Current.Job.log job "Cancel failed: %a" Capnp_rpc.Error.pp e
    in
    let rec aux () =
      sched ~job t >>= fun sched ->
      let urgent =
        match t.urgent with
        | `Always -> true
        | `Never -> false
        | `Auto -> priority = `High
      in
      let ticket = Cluster_api.Submission.submit ~urgent ?src sched ~pool ~action ~cache_hint in
      ticket_ref := Some ticket;
      Current.Switch.add_hook_or_exec switch (fun () ->
          match !ticket_ref with
          | None -> Lwt.return_unit
          | Some ticket -> Capability.dec_ref ticket; ticket_ref := None; Lwt.return_unit
        ) >>= fun () ->
      let build_job = Cluster_api.Ticket.job ticket in
      Capability.wait_until_settled ticket >>= fun () ->
      Current.Job.log job "Job submitted to scheduler. Waiting for worker...";
      Capability.wait_until_settled build_job >>= fun () ->
      Capability.dec_ref ticket;
      ticket_ref := None;  (* Transfer ownership of build_job to caller. *)
      match Capability.problem build_job with
      | None -> Lwt.return build_job
      | Some _ ->
        Lwt.pause () >>= fun () ->
        if Capability.problem sched = None then (
          (* The job failed but we're still connected to the scheduler. Report the error. *)
          Lwt.return build_job
        ) else (
          aux ()
        )
    in
    aux (), cancel

  (* Convert a list of commits in the same repository to a [(repo, hashes)] pair.
     Raises an exception if there are commits from different repositories. *)
  let single_repo = function
    | [] -> None
    | x :: _ as commits ->
      let repo = Git.Commit_id.repo x in
      let hash y =
        let repo' = Git.Commit_id.repo y in
        if repo = repo' then Git.Commit_id.hash y
        else
          Fmt.failwith "Commits from different repositories not supported (%S vs %S)" repo repo'
      in
      Some (repo, List.map hash commits)

  let build t job { Key.dockerfile; src; options; pool; push_target } =
    let push_to =
      match push_target, t.push_auth with
      | Some target, Some (user, password) ->
        Current.Job.log job "Will push staging image to %a" Cluster_api.Docker.Image_id.pp target;
        Some { Cluster_api.Docker.Spec.target; user; password }
      | _ ->
        None
    in
    let action = Cluster_api.Submission.docker_build ?push_to ~options dockerfile in
    let src = single_repo src in
    let cache_hint =
      match t.cache_hint with
      | Some hint -> hint               (* User-supplied cache hint *)
      | None ->
        match src, dockerfile with
        | Some (repo, _), `Contents _ -> repo        (* Group by source repository *)
        | Some (repo, _), `Path path -> Printf.sprintf "%s-%s" repo path        (* Repo-Dockerfile *)
        | None, `Path path -> path          (* Group by Dockerfile path *)
        | None, `Contents contents ->
          (* No repository; use first FROM line. *)
          let from_line x = Astring.String.is_prefix ~affix:"from" (String.lowercase_ascii x) in
          match List.find_opt from_line (String.split_on_char '\n' contents) with
          | Some line -> line
          | None -> ""
    in
    Current.Job.log job "Using cache hint %S" cache_hint;
    let build_pool = Current.Pool.of_fn ~label:"OCluster" @@ submit ~job ~pool ~action ~cache_hint ?src t in
    let level = if push_target = None then Current.Level.Average else Current.Level.Above_average in
    Current.Job.start_with ~pool:build_pool job ?timeout:t.timeout ~level >>= fun build_job ->
    Capability.with_ref build_job @@ fun build_job ->
    let on_cancel _ =
      Cluster_api.Job.cancel build_job >|= function
      | Ok () -> ()
      | Error (`Capnp e) -> Current.Job.log job "Cancel failed: %a" Capnp_rpc.Error.pp e
    in
    Current.Job.with_handler job ~on_cancel @@ fun () ->
    let result = Cluster_api.Job.result build_job in
    tail ~job build_job >>!= fun () ->
    result >>= function
    | Error (`Capnp e) -> Lwt_result.fail (`Msg (Fmt.to_to_string Capnp_rpc.Error.pp e))
    | Ok repo_id ->
      begin match push_target with
        | Some target when t.push_auth = None ->
          Current.Job.log job "Not pushing to %a as staging password not configured"
            Cluster_api.Docker.Image_id.pp target
        | _ -> ()
      end;
      Lwt_result.return repo_id

  let pp f {Key.dockerfile; src; options = _; pool; push_target = _} =
    match dockerfile with
    | `Path path ->
      Fmt.pf f "@[<v2>Build %s using %s in@,%a@]"
        path pool
        (Fmt.Dump.list Git.Commit_id.pp) src
    | `Contents _ ->
      Fmt.pf f "@[<v2>Build using %s in@,%a@]"
        pool
        (Fmt.Dump.list Git.Commit_id.pp) src

  let auto_cancel = true
end

module Build = Current_cache.Make(Op)

open Current.Syntax

let v ?timeout ?push_auth ?(urgent=`Auto) sr =
  let connection = { sr; sched = Lwt.fail_with "init" } in
  { connection; timeout; push_auth; cache_hint = None; urgent }

let label dockerfile pool =
  match dockerfile with
  | `Path path -> Current.component "build %s@,%s" path pool
  | `Contents _ -> Current.component "build@,%s" pool

module Raw = struct
  let with_hint ~cache_hint t =
    match cache_hint with
    | Some _ -> { t with cache_hint }
    | None -> t

  let build_and_push ?cache_hint t ~push_target ~pool ~src ~options dockerfile =
    let t = with_hint ~cache_hint t in
    Build.get t { Op.Key.dockerfile; src; options; pool; push_target = Some push_target }
    |> Current.Primitive.map_result @@ function
    | Ok "" -> Error (`Msg "No output image (push auth not configured)")
    | Ok x -> Ok x
    | Error _ as e -> e

  let build ?cache_hint t ~pool ~src ~options dockerfile =
    let t = with_hint ~cache_hint t in
    Build.get t { Op.Key.dockerfile; src; options; pool; push_target = None }
    |> Current.Primitive.map_result (Result.map (function
        | "" -> ()
        | x -> Fmt.failwith "BUG: got a RepoID (%S) but we didn't ask to push!" x
      ))
end

let unwrap = function
  | `Path _ as x -> Current.return x
  | `Contents x -> Current.map (fun x -> `Contents x) x

let build_and_push ?cache_hint t ~push_target ~pool ~src ~options dockerfile =
  label dockerfile pool |>
  let> dockerfile = unwrap dockerfile
  and> src = src in
  Raw.build_and_push ?cache_hint t ~push_target ~pool ~src ~options dockerfile

let build ?cache_hint t ~pool ~src ~options dockerfile =
  label dockerfile pool |>
  let> dockerfile = unwrap dockerfile
  and> src = src in
  Raw.build ?cache_hint t ~pool ~src ~options dockerfile

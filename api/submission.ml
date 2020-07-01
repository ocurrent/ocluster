open Capnp_rpc_lwt

let error_msg fmt =
  fmt |> Fmt.kstrf @@ fun x -> Error (`Msg x)

module Target : sig
  type t

  val of_string : string -> (t, [> `Msg of string]) result
  val to_string : t -> string
  val repo : t -> string
  val tag : t -> string
  val pp : t Fmt.t
end = struct
  type t = { repo : string; tag : string }

  let of_string s =
    match Astring.String.cut ~rev:true ~sep:":" s with
    | None -> error_msg "Missing ':TAG' in target %S" s
    | Some ("", _) -> error_msg "Repository empty in %S" s
    | Some (_, "") -> error_msg "Tag empty in %S" s
    | Some (repo, tag) -> Ok { repo; tag }

  let to_string { repo; tag } = Printf.sprintf "%s:%s" repo tag

  let repo t = t.repo
  let tag t = t.tag
  let pp f { repo; tag } = Fmt.pf f "%s:%s" repo tag
end
  
module Docker_build = struct
  type target = {
    target : Target.t;
    user : string;
    password : string;
  }

  type t = {
    dockerfile : string;
    push_to : target option;
  }

  let init b { dockerfile; push_to } =
    let module DB = Raw.Builder.DockerBuild in
    DB.dockerfile_set b dockerfile;
    push_to |> Option.iter (fun { target; user; password } ->
        DB.push_target_set b (Target.to_string target);
        DB.push_user_set b user;
        DB.push_password_set b password;
      )

  let read r =
    let module R = Raw.Reader.DockerBuild in
    let dockerfile = R.dockerfile_get r in
    let target = R.push_target_get r in
    let user = R.push_user_get r in
    let password = R.push_password_get r in
    let push_to =
      match target, user, password with
      | "", "", "" -> None
      | "", _, _
      | _, "", _
      | _, _, "" -> Fmt.failwith "push-target, push-user and push-password must be given (or not given) together"
      | target, user, password ->
        match Target.of_string target with
        | Ok target -> Some { target; user; password }
        | Error (`Msg m) -> failwith m
    in
    { dockerfile; push_to }
end

let local ~submit =
  let module X = Raw.Service.Submission in
  X.local @@ object
    inherit X.service

    method submit_impl params release_param_caps =
      let open X.Submit in
      release_param_caps ();
      let pool = Params.pool_get params in
      let descr = Params.descr_get params in
      let urgent = Params.urgent_get params in
      let job = submit ~pool ~urgent descr in
      let response, results = Service.Response.create Results.init_pointer in
      Results.job_set results (Some job);
      Capability.dec_ref job;
      Service.return response
  end

module X = Raw.Client.Submission

type t = X.t Capability.t

type action =
  | Docker_build of Docker_build.t

let docker_build ?push_to dockerfile =
  Docker_build { Docker_build.dockerfile; push_to }

let get_action descr =
  let module JD = Raw.Reader.JobDescr in
  let action = JD.docker_build_get descr in
  Docker_build (Docker_build.read action)

let submit ?src ?(urgent=false) t ~pool ~action ~cache_hint =
  let open X.Submit in
  let module JD = Raw.Builder.JobDescr in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.pool_set params pool;
  Params.urgent_set params urgent;
  let b = Params.descr_get params in
  let Docker_build action = action in
  let db = JD.docker_build_get b in
  Docker_build.init db action;
  JD.cache_hint_set b cache_hint;
  src |> Option.iter (fun (repo, commits) ->
      let _ : _ Capnp.Array.t = JD.commits_set_list b commits in
      JD.repository_set b repo;
    );
  Capability.call_for_caps t method_id request Results.job_get_pipelined

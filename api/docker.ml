let error_msg fmt =
  fmt |> Fmt.kstr @@ fun x -> Error (`Msg x)

module Image_id = struct
  type t = { repo : string; tag : string }

  let v_opt ~repo ~tag =
    match repo, tag with
    | "", _ -> Error (`Msg "Repository empty")
    | _, "" -> Error (`Msg "Tag empty")
    | _, tag when String.contains tag ':' -> Error (`Msg "':' cannot appear in tag!")
    | repo, tag -> Ok { repo; tag }

  let v ~repo ~tag =
    match v_opt ~repo ~tag with
    | Ok t -> t
    | Error (`Msg m) -> failwith m

  let of_string s =
    match Astring.String.cut ~rev:true ~sep:":" s with
    | None -> error_msg "Missing ':TAG' in target %S" s
    | Some (repo, tag) ->
      v_opt ~repo ~tag
      |> Result.map_error (fun (`Msg m) -> `Msg (Fmt.str "%s in %S" m s))

  let repo t = t.repo
  let tag t = t.tag
  let to_string { repo; tag } = Printf.sprintf "%s:%s" repo tag
  let pp f { repo; tag } = Fmt.pf f "%s:%s" repo tag
end

module Spec = struct
  type push = {
    target : Image_id.t;
    auth : (string * string) option;
  }

  type options = {
    build_args : string list;
    nontriggering_build_args : string list;
    squash : bool;
    buildkit: bool;
    include_git : bool [@default true];
  } [@@deriving yojson]

  type t = {
    dockerfile : [`Contents of string | `Path of string];
    options : options;
    push_to : push option;
  }

  let defaults = {
    build_args = [];
    nontriggering_build_args = [];
    squash = false;
    buildkit = false;
    include_git = false;
  }

  let init b { dockerfile; options; push_to } =
    let module DB = Raw.Builder.DockerBuild in
    let module Dockerfile = Raw.Builder.DockerBuild.Dockerfile in
    begin
      let dockerfile_b = DB.dockerfile_get b in
      match dockerfile with
      | `Contents contents -> Dockerfile.contents_set dockerfile_b contents
      | `Path path -> Dockerfile.path_set dockerfile_b path
    end;
    let { build_args; nontriggering_build_args; squash; buildkit; include_git } = options in
    DB.build_args_set_list b build_args |> ignore;
    DB.nontriggering_build_args_set_list b nontriggering_build_args |> ignore;
    DB.squash_set b squash;
    DB.buildkit_set b buildkit;
    DB.include_git_set b include_git;
    push_to |> Option.iter (fun { target; auth } ->
        DB.push_target_set b (Image_id.to_string target);
        Option.iter (fun (user, password) ->
          DB.push_user_set b user;
          DB.push_password_set b password;
        ) auth;
      )

  let read r =
    let module R = Raw.Reader.DockerBuild in
    let dockerfile =
      let module Dockerfile = Raw.Reader.DockerBuild.Dockerfile in
      match Dockerfile.get (R.dockerfile_get r) with
      | Contents c -> `Contents c
      | Path p -> `Path p
      | Undefined _ -> Fmt.failwith "Unknown Dockerfile file"
    in
    let target = R.push_target_get r in
    let user = R.push_user_get r in
    let password = R.push_password_get r in
    let build_args = R.build_args_get_list r in
    let nontriggering_build_args = R.nontriggering_build_args_get_list r in
    let squash = R.squash_get r in
    let buildkit = R.buildkit_get r in
    let include_git = R.include_git_get r in
    let options = { build_args; nontriggering_build_args; squash; buildkit; include_git } in
    let push_to =
      match target, user, password with
      | "", "", "" -> None
      | "", _, _ -> Fmt.failwith "push-user and push-password must be given with push-target"
      | target, user, password ->
        if (user = "") <> (password = "") then
          Fmt.failwith "push-user and push-password must be given (or not given) together"
        else
          let auth = if user = "" then None else Some (user, password) in
          match Image_id.of_string target with
          | Ok target -> Some { target; auth }
          | Error (`Msg m) -> failwith m
    in
    { dockerfile; options; push_to }
end

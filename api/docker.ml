let error_msg fmt =
  fmt |> Fmt.kstrf @@ fun x -> Error (`Msg x)

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
      |> Result.map_error (fun (`Msg m) -> `Msg (Fmt.strf "%s in %S" m s))

  let repo t = t.repo
  let tag t = t.tag
  let to_string { repo; tag } = Printf.sprintf "%s:%s" repo tag
  let pp f { repo; tag } = Fmt.pf f "%s:%s" repo tag
end

module Spec = struct
  type push = {
    target : Image_id.t;
    user : string;
    password : string;
  }

  type options = {
    build_args : string list;
    squash : bool;
    buildkit: bool;
  } [@@deriving yojson]

  type t = {
    dockerfile : [`Contents of string | `Path of string];
    options : options;
    push_to : push option;
  }

  let defaults = {
    build_args = [];
    squash = false;
    buildkit = false;
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
    let { build_args; squash; buildkit } = options in
    DB.build_args_set_list b build_args |> ignore;
    DB.squash_set b squash;
    DB.buildkit_set b buildkit;
    push_to |> Option.iter (fun { target; user; password } ->
        DB.push_target_set b (Image_id.to_string target);
        DB.push_user_set b user;
        DB.push_password_set b password;
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
    let squash = R.squash_get r in
    let buildkit = R.buildkit_get r in
    let options = { build_args; squash; buildkit } in
    let push_to =
      match target, user, password with
      | "", "", "" -> None
      | "", _, _
      | _, "", _
      | _, _, "" -> Fmt.failwith "push-target, push-user and push-password must be given (or not given) together"
      | target, user, password ->
        match Image_id.of_string target with
        | Ok target -> Some { target; user; password }
        | Error (`Msg m) -> failwith m
    in
    { dockerfile; options; push_to }
end

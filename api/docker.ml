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

  type t = {
    dockerfile : string;
    build_args : string list;
    push_to : push option;
  }

  let init b { dockerfile; build_args; push_to } =
    let module DB = Raw.Builder.DockerBuild in
    DB.dockerfile_set b dockerfile;
    DB.build_args_set_list b build_args |> ignore;
    push_to |> Option.iter (fun { target; user; password } ->
        DB.push_target_set b (Image_id.to_string target);
        DB.push_user_set b user;
        DB.push_password_set b password;
      )

  let read r =
    let module R = Raw.Reader.DockerBuild in
    let dockerfile = R.dockerfile_get r in
    let target = R.push_target_get r in
    let user = R.push_user_get r in
    let password = R.push_password_get r in
    let build_args = R.build_args_get_list r in
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
    { dockerfile; build_args; push_to }
end

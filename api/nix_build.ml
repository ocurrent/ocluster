open Lwt.Infix

module Spec = struct
  type drv = [`Derivation of string] [@@deriving to_yojson]
  
  type cmd = {
    drv : drv;
    exe : string;
    args: string list;
  } [@@deriving to_yojson]

  type t =
    | Eval of string
    | Build of string
    | Run of cmd
  [@@deriving to_yojson]

  let init_drv b { filename; drv } =
    let module B = Raw.Builder.NixDerivation in
    let () = match filename with
    | `Filename filename -> B.filename_set b filename
    in
    let () = match drv with
    | `Contents drv -> B.drv_set b drv
    in
    ()

  let init b t =
    let module B = Raw.Builder.NixBuild in
    let module A = Raw.Builder.NixBuild.Action in
    let b = B.action_init b in
    begin match t with
      | Eval expr -> A.eval_set b expr
      | Build build ->
        let b = A.build_init b in
        init_drv b build
      | Run { drv; exe; args } ->
        let b = A.run_init b in
        let module B = Raw.Builder.NixCommand in
        let () = B.exe_set b exe in
        let _ = B.cmd_set_list b args in
        let db = B.drv_init b in
        let () = init_drv db drv in
        ()
    end
  
  let read_drv r =
    let module R = Raw.Reader.NixDerivation in
    let filename = `Filename (R.filename_get r) in
    let drv = `Contents (R.drv_get r) in
    { filename; drv }

  let read r =
    let module R = Raw.Reader.NixBuild in
    let module RA = Raw.Reader.NixBuild.Action in
    match R.action_get r |> R.Action.get with
      | Eval expr -> Eval expr
      | Build drv -> Build (read_drv drv)
      | Run cmd ->
        let module RC = Raw.Reader.NixCommand in
        let drv = RC.drv_get cmd |> read_drv in
        let exe = RC.exe_get cmd in
        let args = RC.cmd_get_list cmd in
        Run { drv; exe; args }
      | Undefined x -> Fmt.failwith "Unknown action type %d" x


  let pp_drv f { filename = `Filename filename; _ } = Fmt.string f filename

  let pp f = function
    | Build drv -> pp_drv f drv
    | Run { drv; exe; _; } ->
      pp_drv f drv;
      Fmt.char f '/';
      Fmt.string f exe
    | Eval expr -> Fmt.string f expr
  
  let from_file path =
    let filename = Filename.basename path |> Str.replace_first (Str.regexp "[^-]+-") "" in
    Lwt_io.(with_file ~mode:input) path (Lwt_io.read ?count:None) >|= fun contents ->
    {
      filename = `Filename filename;
      drv = `Contents contents;
    }
end


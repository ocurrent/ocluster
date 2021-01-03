module Spec = struct
  type t = {
    filename : [`Filename of string];
    drv : [`Contents of string]
  } [@@deriving to_yojson]

  let init b { filename; drv } =
    let module B = Raw.Builder.NixBuild in
    let () = match filename with
    | `Filename filename -> B.filename_set b filename
    in
    let () = match drv with
    | `Contents drv -> B.drv_set b drv
    in
    ()

  let read r =
    let module R = Raw.Reader.NixBuild in
    let filename = `Filename (R.filename_get r) in
    let drv = `Contents (R.drv_get r) in
    { filename; drv }

  let pp f { filename = `Filename filename; _ } = Fmt.string f filename
end


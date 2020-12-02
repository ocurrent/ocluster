module Spec = struct
  type t = {
    spec : [`Contents of string]
  } [@@deriving yojson]

  let init b { spec } =
    let module B = Raw.Builder.OBuilder in
    match spec with
    | `Contents contents -> B.spec_set b contents

  let read r =
    let module R = Raw.Reader.OBuilder in
    let spec = `Contents (R.spec_get r) in
    { spec }
end

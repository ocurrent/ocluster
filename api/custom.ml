type payload = Raw.Reader.pointer_t

type t = {
  kind : string;
  payload : payload;
}

let v ~kind payload = { kind; payload }

let kind t = t.kind
let payload t = t.payload

let read (action : Raw.Reader.Custom.t) =
  let payload = Raw.Reader.Custom.payload_get action in
  let kind = Raw.Reader.Custom.kind_get action in
  { kind; payload }

let init b { kind; payload } =
  Raw.Builder.Custom.kind_set b kind;
  let _ : Raw.Builder.pointer_t = Raw.Builder.Custom.payload_set_reader b payload in
  ()

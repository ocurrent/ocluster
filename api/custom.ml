type payload = Raw.Reader.pointer_t

type 'a t = {
  kind : string;
  payload : 'a;
}

type send = (Raw.Builder.pointer_t -> unit) t
type recv = Raw.Reader.pointer_t t

let v ~kind payload = { kind; payload }

let kind t = t.kind
let payload t = t.payload

let read (action : Raw.Reader.Custom.t) =
  let payload = Raw.Reader.Custom.payload_get action in
  let kind = Raw.Reader.Custom.kind_get action in
  { kind; payload }

let init b { kind; payload } =
  Raw.Builder.Custom.kind_set b kind;
  payload (Raw.Builder.Custom.payload_get b)

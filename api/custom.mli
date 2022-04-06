type payload = Raw.Reader.pointer_t
(** The custom job specification payload. *)

type 'a t
(** Custom job specifications *)

type send = (Raw.Builder.pointer_t -> unit) t
type recv = Raw.Reader.pointer_t t

val v : kind:string -> (Raw.Builder.pointer_t -> unit) -> send
(** [v ~kind payload] is a custom job specification. *)

val kind : _ t -> string
(** A string describing the kind of custom job. *)

val payload : 'a t -> 'a
(** The dynamic payload of the custom job. *)

val init : Raw.Builder.Custom.t -> send -> unit
(** [init builder t] initialises a fresh builder with the values from [t]. *)

val read : Raw.Reader.Custom.t -> recv
(** [read c] reads the buffer and returns a custom job specification. *)

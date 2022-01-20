type t
(** Custom job specifications *)

val v : kind:string -> Raw.Reader.pointer_t -> t
(** [v ~kind payload] is a custom job specific*)

val kind : t -> string
(** A string describing the kind of custom job *)

val payload : t -> Raw.Reader.pointer_t
(** The dynamic payload of the custom job *)

val init : Raw.Builder.Custom.t -> t -> unit
(** [init builder t] initialises a fresh builder with the values from [t]. *)

val read : Raw.Reader.Custom.t -> t
(** [read c] reads the buffer and returns a custom job specification. *)

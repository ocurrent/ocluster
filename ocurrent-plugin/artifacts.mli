type prep

type t

val create : ?expire_after:Duration.t -> code:string -> string -> (t, [> `Msg of string]) Lwt_result.t

val extract : folder:string -> Obuilder_spec.t -> Obuilder_spec.t

val id : t -> string

val public_path : t -> Fpath.t

val store : Fpath.t
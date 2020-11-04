include Capnp_rpc_net.Restorer.LOADER

module Ty : sig
  (** The database can store different types of resource.
      It uses strings to identify them, but the rest of the code uses OCaml types.
      This module maintains a mapping between type names and OCaml types. *)

  type t = ..

  val register : string -> t -> unit
  (** [register_type name ty] registers [name] as the string representation of [ty].
      A single database may be used to store resources from several libraries, so try to pick
      a unique type name. *)

  val pp : t Fmt.t
end

type descr = (Ty.t * string)
(** A [descr] contains the information stored in the database which is used to restore a capability.
    This string are the "arguments", passed to the user's [load] function. *)

type digest = private string

val create :
  make_sturdy:(Capnp_rpc_net.Restorer.Id.t -> Uri.t) ->
  load:(validate:(unit -> bool) ->
        sturdy_ref:'a Capnp_rpc_lwt.Sturdy_ref.t ->
        descr ->
        Capnp_rpc_net.Restorer.resolution Lwt.t
       ) ->
  Db.t ->
  t
(** [create ~make_sturdy ~load db] is a loader that persists information about sturdy refs in an sqlite3 database.
    [make_sturdy] converts a secret ID to a sturdy URI.
    Use e.g. {!Capnp_rpc_unix.Vat_config.sturdy_uri} for this.
    When a request for a previously-issued sturdy ref arrives,
    the loader gets the type and arguments for the object from the database
    and then calls [load ~validate ~sturdy_ref descr] to create the live ref.
    [sturdy_ref] is the object's own sturdy-ref, which it may want to hand back to clients
    (e.g. if implementing the persistence API).
    [validate] can be used to check whether the object is still in the table, to handle revocation.
*)

val add : t -> descr -> Capnp_rpc_net.Restorer.Id.t
(** [add t descr] generates and returns new secret ID. It also adds [descr] to the database under the hash of this secret.
    If a user later uses the secret to connect, [descr] will be used to load the object. *)

val remove : t -> digest -> unit
(** [remove t hash] removes the entry with the given hash, if any.
    @raise Not_found if [digest] is not present. *)

val list_by_type : t -> Ty.t -> (digest * string) list
(** [list_by_type t ty] returns all entries [(digest, args)] with the given [ty]. *)

val lookup_by_descr : t -> descr -> digest list
(** [lookup_by_descr t descr] returns all entries with the given type and arguments. *)

open Capnp_rpc_lwt

module Secret = Capnp_rpc_net.Restorer.Id

module Ty = struct
  type t = ..

  let by_name = Hashtbl.create 10
  let by_value = Hashtbl.create 10

  let register name t =
    if Hashtbl.mem by_name name then Fmt.invalid_arg "%S already a type name!" name
    else if Hashtbl.mem by_value t then Fmt.invalid_arg "Type already registered"
    else (
      Hashtbl.add by_name name t;
      Hashtbl.add by_value t name
    )

  let to_string : t -> string = Hashtbl.find by_value
  let of_string : string -> t = Hashtbl.find by_name

  let pp = Fmt.using (Hashtbl.find_opt by_value) Fmt.(option ~none:(any "(unregistered type)") string)
end

type digest = string

type descr = (Ty.t * string) (** (type, args) *)

type t = {
  db : Sqlite3.db;
  make_sturdy : Secret.t -> Uri.t;
  load : validate:(unit -> bool) -> sturdy_ref:([`Generic] Sturdy_ref.t) -> descr -> Capnp_rpc_net.Restorer.resolution Lwt.t;
  add : Sqlite3.stmt;
  remove : Sqlite3.stmt;
  lookup_by_hash : Sqlite3.stmt;
  lookup_by_descr : Sqlite3.stmt;
  list_by_type : Sqlite3.stmt;
}

let create ~make_sturdy ~load db =
  Sqlite3.exec db "CREATE TABLE IF NOT EXISTS sturdy_refs ( \
                   hash       BLOB NOT NULL, \
                   type       TEXT NOT NULL, \
                   args       BLOB NOT NULL, \
                   created    DATETIME NOT NULL, \
                   PRIMARY KEY (hash))" |> Db.or_fail ~cmd:"create table";
  Sqlite3.exec db "CREATE INDEX IF NOT EXISTS sturdy_refs_rev ON sturdy_refs (type, args)" |> Db.or_fail ~cmd:"create sturdy_refs_rev";
  let add = Sqlite3.prepare db "INSERT INTO sturdy_refs (hash, type, args, created) VALUES (?, ?, ?, date('now'))" in
  let remove = Sqlite3.prepare db "DELETE FROM sturdy_refs WHERE hash = ?" in
  let lookup_by_hash = Sqlite3.prepare db "SELECT type, args FROM sturdy_refs WHERE hash = ?" in
  let lookup_by_descr = Sqlite3.prepare db "SELECT hash FROM sturdy_refs WHERE type = ? AND args = ?" in
  let list_by_type = Sqlite3.prepare db "SELECT hash, args FROM sturdy_refs WHERE type = ? ORDER BY args" in
  let load ~validate ~sturdy_ref descr = load ~validate ~sturdy_ref:(Sturdy_ref.cast sturdy_ref) descr in
  { db; make_sturdy; load; add; remove; lookup_by_hash; lookup_by_descr; list_by_type }

let lookup_by_hash t hash =
  match Db.query_some t.lookup_by_hash Sqlite3.Data.[ BLOB hash ] with
  | Some Sqlite3.Data.[TEXT ty; BLOB args] -> Some (Ty.of_string ty, args)
  | Some row -> Fmt.failwith "lookup_by_hash: bad row: %a" Db.dump_row row
  | None -> None

let lookup_by_descr t (ty, args) =
  let ty = Ty.to_string ty in
  Db.query t.lookup_by_descr Sqlite3.Data.[ TEXT ty; BLOB args ] |> List.map @@ function
  | Sqlite3.Data.[BLOB hash] -> hash
  | row -> Fmt.failwith "lookup_by_args: bad row: %a" Db.dump_row row

let list_by_type t ty =
  let ty = Ty.to_string ty in
  Db.query t.list_by_type Sqlite3.Data.[ TEXT ty ]
  |> List.map @@ function
  | Sqlite3.Data.[BLOB hash; BLOB args] -> hash, args
  | row -> Fmt.failwith "list_by_type: bad row: %a" Db.dump_row row

let hash _ = `SHA256

let add t (ty, args) =
  let ty = Ty.to_string ty in
  let secret = Secret.generate () in
  let digest = Secret.digest (hash t) secret in
  Db.exec t.add Sqlite3.Data.[ BLOB digest; TEXT ty; BLOB args ];
  secret

let remove t digest =
  Db.exec t.remove Sqlite3.Data.[ BLOB digest ];
  if Sqlite3.changes t.db = 0 then raise Not_found

let validate t digest () =
  match lookup_by_hash t digest with
  | None -> false
  | Some _ -> true

let load t self digest =
  match lookup_by_hash t digest with
  | None -> Lwt.return Capnp_rpc_net.Restorer.unknown_service_id
  | Some descr -> t.load ~validate:(validate t digest) ~sturdy_ref:(Capnp_rpc_lwt.Sturdy_ref.cast self) descr

let make_sturdy t = t.make_sturdy

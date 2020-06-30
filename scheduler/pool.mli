module Dao : sig
  type t

  val init : Sqlite3.db -> t
  (** Ensure the required tables are created. *)
end

module Make (Item : S.ITEM) : sig
  type t
  (** A pool of workers and queued jobs. *)

  type worker
  (** A connected worker. *)

  val create : name:string -> db:Dao.t -> t
  (** [create ~name ~db] is a pool that reports metrics tagged with [name] and
      stores cache information in [db]. *)

  val register : t -> name:string -> (worker, [> `Name_taken]) result
  (** [register t ~name] returns a queue for worker [name]. *)

  val submit : urgent:bool -> t -> Item.t -> unit
  (** [submit ~urgent t item] adds [item] to the incoming queue.
      [urgent] items will be processed before non-urgent ones. *)

  val pop : worker -> (Item.t, [> `Finished]) Lwt_result.t
  (** [pop worker] gets the next item for [worker]. *)

  val release : worker -> unit
  (** [release worker] marks [worker] as disconnected.
      [worker] cannot be used again after this (use [register] to get a new one). *)

  val dump : t Fmt.t
end

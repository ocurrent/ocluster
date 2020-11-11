module Dao : sig
  type t

  val init : Sqlite3.db -> t
  (** Ensure the required tables are created. *)
end

module Make (Item : S.ITEM) (Time : S.TIME) : sig
  type t
  (** A pool of workers and queued jobs. *)

  type ticket
  (** A queued item. *)

  type worker
  (** A connected worker. *)

  module Client : sig
    type t
    (** A connected client. *)
  end

  val create : name:string -> db:Dao.t -> t
  (** [create ~name ~db] is a pool that reports metrics tagged with [name] and
      stores cache information in [db]. *)

  val register : t -> name:string -> capacity:int -> (worker, [> `Name_taken]) result
  (** [register t ~name ~capacity] returns a queue for worker [name].
      @param capacity Worker's capacity (max number of parallel jobs). *)

  val client : t -> client_id:string -> Client.t
  (** [client t ~client_id] is a client value, which can be used to submit jobs.
      These jobs will be scheduled alongside the jobs of other clients, so that
      one client does not starve the others.
      @param [client_id] Used for logging and reporting. *)

  val submit : urgent:bool -> Client.t -> Item.t -> ticket
  (** [submit ~urgent client item] adds [item] to the incoming queue.
      [urgent] items will be processed before non-urgent ones. *)

  val cancel : ticket -> (unit, [> `Not_queued ]) result
  (** [cancel ticket] discards the item from the queue. *)

  val pop : worker -> (Item.t, [> `Finished]) Lwt_result.t
  (** [pop worker] gets the next item for [worker]. *)

  val set_active : worker -> bool -> unit
  (** [set_active worker active] sets the worker's active flag.
      When set to [true], items can be added from the main queue.
      When changed to [false], any entries on the queue are pushed back to the
      main queue, and the queue stops accepting new items.
      If the worker is marked as shutting down then this has no effect. *)

  val is_active : worker -> bool
  (** [is_active worker] returns [worker]'s active flag. *)

  val shutdown : worker -> unit
  (** [shutdown worker] marks [worker] as shutting down. The worker is
      set to inactive, and cannot become active again. *)

  val connected_workers : t -> worker Astring.String.Map.t
  (** [connected_workers t] is the set of workers currently connected, whether active or not,
      indexed by name. *)

  val release : worker -> unit
  (** [release worker] marks [worker] as disconnected.
      [worker] cannot be used again after this (use [register] to get a new one). *)

  val show : t Fmt.t
  (** [show] shows the state of the system, including registered workers and queued jobs. *)

  val dump : t Fmt.t
  (** [dump] is similar to [show], but also dumps the contents of the database.
      It is probably only useful for unit-tests. *)
end

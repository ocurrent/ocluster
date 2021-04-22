module Dao : sig
  type t

  val init : Sqlite3.db -> t
  (** Ensure the required tables are created. *)
end

(** Why a worker queue is inactive. *)
module Inactive_reasons : sig
  type t
  (** A set of reasons. *)

  val worker : t
  (** The worker may pause itself. *)

  val admin_pause : t
  (** The admin paused it. *)

  val admin_shutdown : t
  (** The admin is shutting it down. *)

  val mem : t -> t -> bool
  (** [mem a b] is [true] if [a] is in the set [b] (is a subset of [b]). *)

  val pp : t Fmt.t
end

module Make (Item : S.ITEM) (Time : S.TIME) : sig
  type t
  (** A pool of workers and queued jobs. *)

  type ticket
  (** A queued item. *)

  type worker
  (** A connected worker. *)

  module Worker_map : Map.S with type key = string

  module Client : sig
    type t
    (** A connected client. *)

    val submit : urgent:bool -> t -> Item.t -> ticket
    (** [submit ~urgent t item] adds [item] to the incoming queue.
        [urgent] items will be processed before non-urgent ones. *)

    val cancel : t -> ticket -> (unit, [> `Not_queued ]) result
    (** [cancel t ticket] discards the item from the queue. *)

    val set_rate : t -> float -> unit
    (** [set_rate t rate] sets the maximum number of jobs that the client can expect
        to run at once. Clients can submit more jobs than this, and make use of any
        spare capacity. However, this will determine what happens when multiple clients
        want to use the extra capacity. *)

    val get_rate : t -> float
    (** [get_rate t] is the rate previously set by [set_rate] (or [1.0] if never set). *)

    val client_id : t -> string
    val pool_id : t -> string
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

  val remove_client : t -> client_id:string -> unit
  (** [remove_client t ~client_id] deletes all information about [client_id], if any.
      Call this on all pools when deleting a user. *)

  val pop : worker -> (Item.t, [> `Finished]) Lwt_result.t
  (** [pop worker] gets the next item for [worker]. *)

  val job_finished : worker -> unit
  (** [job_finished worker] is called when [worker] completes a job. *)

  val running_jobs : ?prev:int -> worker -> int Lwt.t
  (** [running_jobs worker] returns the number of jobs running on [worker].
      @param prev Wait until the number is different to [prev] before returning. *)

  val set_active : reason:Inactive_reasons.t -> worker -> bool -> unit
  (** [set_active ~reason worker active] sets the worker's active flag for [reason].
      A worker is active if it has no reasons to be inactive.
      When active, items can be added from the main queue.
      When inactive, any entries on the queue are pushed back to the
      main queue, and the queue stops accepting new items. *)

  val inactive_reasons : worker -> Inactive_reasons.t
  (** [inactive_reasons worker] is the set of reasons why [worker]'s queue is inactive, if any. *)

  val is_active : worker -> bool
  (** [is_active worker] is [inactive_reasons worker = empty]. *)

  val shutdown : worker -> unit
  (** [shutdown worker] marks [worker] as shutting down. The worker is
      set to inactive, and cannot become active again. *)

  val connected_workers : t -> worker Worker_map.t
  (** [connected_workers t] is the set of workers currently connected, whether active or not,
      indexed by name. *)

  val known_workers : t -> Cluster_api.Pool_admin.worker_info list
  (** [known_workers t] lists all workers known to the pool, whether connected or not. *)

  val with_worker : t -> string -> (worker -> 'a) -> 'a
  (** [with_worker t name f] is [f worker] if [worker] is in [connected_workers].
      If not, it temporarily registers it, runs [f], and then releases it. *) 

  val worker_known : t -> string -> bool
  (** [worker_known t name] is [true] when worker [name] has previously registered with this pool,
      whether currently connected or not. *)

  val release : worker -> unit
  (** [release worker] marks [worker] as disconnected.
      [worker] cannot be used again after this (use [register] to get a new one). *)

  val forget_worker : t -> string -> (unit, [`Unknown_worker | `Still_connected]) result
  (** [forget_worker t name] removes [name] from the set of known workers. *)

  val show : t Fmt.t
  (** [show] shows the state of the system, including registered workers and queued jobs. *)

  val dump : t Fmt.t
  (** [dump] is similar to [show], but also dumps the contents of the database.
      It is probably only useful for unit-tests. *)
end

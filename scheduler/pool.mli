module Make (Item : sig type t end) : sig
  type t
  (** A pool of workers and queued jobs. *)

  type worker
  (** A connected worker. *)

  val create : unit -> t

  val register : t -> name:string -> (worker, [> `Name_taken]) result
  (** [register t ~name] returns a queue for worker [name]. *)

  val submit : t -> Item.t -> unit
  (** [submit t item] adds [item] to the incoming queue. *)

  val pop : worker -> (Item.t, [> `Finished]) Lwt_result.t
  (** [pop worker] gets the next item for [worker]. *)

  val release : t -> worker -> unit
  (** [release t worker] marks [worker] as disconnected.
      It cannot be used again after this (use [register] to get a new one). *)
end

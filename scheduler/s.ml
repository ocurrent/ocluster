type cost = {
  non_cached : int;             (* Cost of building on a machine which hasn't built something like this before. *)
  cached : int;                 (* Cost of rebuilding on the same machine. *)
}

module type ITEM = sig
  type t

  type cache_hint = private string

  val cache_hint : t -> cache_hint
  (** Try to schedule items with the same cache hint on the same machine. *)

  val cost_estimate : t -> cost

  val pp : t Fmt.t
  (** For debugging. *)
end

module type TIME = sig
  val gettimeofday : unit -> float
end

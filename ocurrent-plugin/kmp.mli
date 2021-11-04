(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*S Knuth-Morris-Pratt string search algorithm. *)

(*s [search p t] searches the first occurrence of pattern
    [p] in text [t]. It raises [Not_found] if [p] does not occur in [t].
    Strings are 0-based. Both arguments can be empty.
    If [p] is to be searched many times in different texts then a partial
    application [search p] is more efficient (the shift table is computed
    only once).

    Complexity: if [m] is the length of [p] and [n] the length of [t] then
    [search p t] runs in time $O(m+n)$ and uses $O(m)$ space.
*)

val search : string -> string -> int

(*s Functorial interface. Knuth-Morris-Pratt algorithm can be applied
    to patterns and texts of any type using the following functor. 
    Patterns and texts may be of different types, as soon as the type of 
    characters is common. [length s] must return the length of [s], and 
    [get s i] must return the [i]-th character of [s] (starting from 0).
    Equality on characters is supposed to be structural equality ([=]).
*)

module type STRING = sig
  type t 
  type char
  val length : t -> int
  val get : t -> int -> char
end

module Make(P : STRING)(T : STRING with type char = P.char) : sig

  val search : P.t -> T.t -> int

end

(*s Debugging: setting the following boolean reference to [true] will
    dump the shift table on error output. *)

val debug : bool ref

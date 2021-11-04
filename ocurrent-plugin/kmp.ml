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

(*s Knuth-Morris-Pratt implementation. The shift table is called [next] here
    and stored in an array. *)

open Printf

let debug = ref false

let search p =
  let m = String.length p in
  let next = Array.create m 0 in
  (* initialization of [next] *)
  let i = ref 1 in
  let j = ref 0 in
  if m > 1 then begin
    while !i < m - 1 do
      if p.[!i] = p.[!j] then begin
  i := !i + 1; j:= !j + 1; next.(!i) <- !j
      end else
  if !j = 0 then i := !i + 1 (* next[i] <- 0 *) else j := next.(!j)
    done
  end;
  (* debug: dump of the [next] table *)
  if !debug then 
    for i = 0 to m - 1 do eprintf "next[%d]=%d\n" i next.(i) done;
  fun t -> 
    (* search in [t] *)
    let n = String.length t in
    i := 0;
    j := 0;
    while !j < m && !i < n do
      if t.[!i] = p.[!j] then begin
  i := !i + 1; j:= !j + 1
      end else
  if !j = 0 then i := !i + 1 else j := next.(!j)
    done;
    if !j = m then !i - m else raise Not_found

(*s Functorial interface. *)

module type STRING = sig
  type t 
  type char
  val length : t -> int
  val get : t -> int -> char
end

module Make(P : STRING)(T : STRING with type char = P.char) = struct

  let search p =
    let m = P.length p in
    let next = Array.create m 0 in
    (* initialization of [next] *)
    let i = ref 1 in
    let j = ref 0 in
    if m > 1 then begin
      while !i < m - 1 do
  if P.get p !i = P.get p !j then begin
    i := !i + 1; j:= !j + 1; next.(!i) <- !j
  end else
    if !j = 0 then i := !i + 1 else j := next.(!j)
      done
    end;
    fun t ->
      (* search in [t] *)
      let n = T.length t in
      i := 0;
      j := 0;
      while !j < m && !i < n do
  if T.get t !i = P.get p !j then begin
    i := !i + 1; j:= !j + 1
  end else
    if !j = 0 then i := !i + 1 else j := next.(!j)
      done;
      if !j = m then !i - m else raise Not_found

end

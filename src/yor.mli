(***********************************************************************)
(*                                                                     *)
(*                            YOR                                      *)
(*                                                                     *)
(*           (C) 2007 by Zheng Li (li@pps.jussieu.fr)                  *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU Library General Public        *)
(*  License as published by the Free Software Foundation; either       *)
(*  version 2 of the License, or (at your option) any later version.   *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU Library General Public License for more details.               *)
(*                                                                     *)
(***********************************************************************)


(** {6 YOR type} *)

type pobj =
    [ `Closure of closure
    | `Double of float
    | `Double_array of float array
    | `Force of pobj
    | `Int of int
    | `Lazy of closure
    | `Object of clobj * int * pobj list
    | `String of string
    | `Tuple of pobj list ]
and closure = codeptr * pobj list
and codeptr = Obj.t
and clobj = Obj.t


(** {6 YOR operations} *)

val parse : 'a -> pobj
val dump : 'a -> string
val print: 'a -> unit
val find: (Obj.t -> bool) -> 'a -> Obj.t option
val iter: (Obj.t -> unit) -> 'a -> unit
val refed_by : (Obj.t -> Obj.t -> bool) -> 'a -> 'b -> bool
val subst : (Obj.t -> Obj.t -> bool) -> 'a * 'a -> 'b -> 'b


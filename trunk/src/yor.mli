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

(** Helper functions dealing with internal representation of OCaml values,
    still ALPHA!!!
*)


(** {6 YOR type} *)

(** a representation format of parsed OCaml value *)
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

(** parse any OCaml value to its corresponding pobj representation *)
val parse : 'a -> pobj

(** [refed_by e x] test whether [e] is physically referenced by [x] *)
val refed_by : 'a -> 'b -> bool

(** [subst (e, e') x] substitue any occurience of [e] inside [x] with [e'] *)
val subst : 'a * 'a -> 'b -> 'b

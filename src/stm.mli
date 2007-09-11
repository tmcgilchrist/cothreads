(***********************************************************************)
(*                                                                     *)
(*                   STM library for OCaml                             *)
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

(** A user-space STM library for vmthreads, system threads, process and network
    programs(TODO).
*)


(** {6 STM model} *)


type 'a stm
  (** The type of a transaction, when executed will produce a result of type
  ['a]. *) 

val return: 'a -> 'a stm
  (** Primitive to wrap a plain of type ['a] value to a ['a stm], which when
      being executed, will produces the orignal value. *)

val bind: 'a stm -> ('a -> 'b stm) -> 'b stm
  (** [bind t f] is a transaction, when executed, first behavior as
      transaction [t], then feed the reture value to [f] to get the
      consecutive transaction to execute next.
  *)

val ( >>= ): 'a stm -> ('a -> 'b stm) -> 'b stm
  (** [t >>= f] is an alternative notation of [bind t f] *)

val ( >> ): 'a stm -> 'b stm -> 'b stm
  (** [t1 >> t2] is equal to [t1 >>= fun _ -> t2] which first execute [t1] and
      wait for its result but ignore it, and then behaviors like [t2]
  *)

val abort: 'a stm
  (** [abort] is a single transaction, when executed, abort the whole execution
      from current point. The result of abort an execution is detailed in the
      documentation of execution primitive [atom_once] below.
  *)

val retry: 'a stm
  (** [retry] is a transaction, when executed, first wait for the changing of
      any transactional variables being read in the history of current
      execution, then relaunch the whole execution.
  *)

val retry_now: 'a stm
  (** [retry_now] is a transaction in the same spirit with [retry], the only
      difference is that it does not wait for any changes and relaunch the
      execution immediately. This can be useful in a language with side-effect,
      see also the documentation on [atom_once]
  *)

val wait: unit stm
  (** [wait] is a transaction, when executed, simply wait for the changing of
      any transactional variables being read in the history of current
      execution, but without relaunch it. Semantically, you can consider [retry]
      as [wait >> retry_now]
  *)

val or_else: 'a stm -> 'a stm -> 'a stm
  (** [or_else t1 t2] is a transaction, when executed, first try to execute
      [t1]. If not encountering any [retry] ([retry_now]) or [abort], it
      behaviors just as [t1], otherwise it try [t2] in the same way. If both
      [t1] and [t2] [abort], the whole execution will abort; if either of them
      [retry_now], the whole execution will relaunch immediately; if either of
      them [retry], the execution will wait on its waiting condition and then
      relaunch; if both of them retry, the execution will wait on both their
      waiting conditions and then relaunch.
  *)

val catch: 'a stm -> (exn -> 'a stm) -> 'a stm
  (** [catch t f] is a transaction, when executed, behaviors as [t] if no
      exception arise, otherwise [f] is used to catch this exception and
      produce the replacing transaction to execute. Note that [catch] is
      transaction-level [try ... with].

      E.g., [let t3 = t1 >>= fun x -> let y = possible_exn x in t2], here [t3] is
      a transaction with potential exception inside,  [try t3 with ...] won't
      catch it because t3 is not executed yet, and [try atom_once t3 with ...]
      can catch it but then [t3] lost its composability. The solution is [catch
      t3 (function _ -> t4)] which catches exceptions {i inside} [t3] and
      results a valid transaction being able to furtherly composed; another
      possibility is to catch the exception normally {i outside} transactions
      like [t1 >>= fun x -> let y = try possible_exn x with _ -> some_value
      in t2]. 
  *)

val atom_once: 'a stm -> 'a option 
  (** [atom_once] execute a transaction and result in [Some v] if the
      transaction success and [None] if the transaction fail (due to
      conflicting in committing or abort). One difference between OCaml and
      Haskell is that OCaml is not pure and can hide side-effect anywhere while
      Haskell is pure and can seperate values with/without side-effect by
      types. On STM, any transaction may fail and relaunch for some times
      before its success, so any side-effects inside the transaction may be
      launched several times. Haskell forbid side effect inside transaction
      through types, whereas we won't be able to do that with OCaml. Instead of
      asking, but have no means to detect or forbid, the programmers to program
      without side-effect inside transaction, or modifying heavily the
      underlying run-time of a language with a imperative nature to be able to
      catch/revert side-effect, we simply tell it from type that "A transaction
      {i may} fail" and let the programmer decide what to do. This kind of
      things already exist in OCaml such as exception. E.g. [try incr i;
      danger_v1 with _ -> decr i; v2], it's the programmers' responsibility to
      revert [i] or choose not to do the side-effect modification inside a
      dangerous envrionment, if that's what they mean. On the other hand, the good 
      thing is that now the programmers have more flexibility in controlling the
      execution of transactions, e.g. they may choose in purpose not to
      repeatedly execute the transaction after the committing fails [x] times.
  *)

val atom: 'a stm -> 'a
  (** This is an analog of [atomically] in Haskell, which repeatedly execute
      a transaction until the committing succeed. As being said in [atom_once],
      the control is given to the programmer, the [atom] can defined by themselves
      as [let rec atom t = match atom_once t with Some v -> v | _ -> atom t]. 
      Providing it is just for convenience. In the same way, you can define various
      helper functions such as [check].

      As already warned (see [atom_once], transactions may fail, relauching 
      transactions also means relaunching side-effects inside a transaction if any.
      So usually you should avoid side-effect, unless it's something you don't care
      or even something you want: as an example, you may want to add a harmless print
      routine inside the transaction to be able to debug that how may times the
      transaction fails before its success :)

      Unlike in haskell, we allow nested [atom_once] or [atom].
  *)


(** {6 Transactional variable} *)

type 'a tvar
  (** the type of transactional variable, which has inside a value of type ['a] *)

val tvar: 'a -> 'a tvar
  (** Toplevel tvar declaration, produce a transaction variable from a
      value. See [new_tvar] *)

val new_tvar: 'a -> 'a tvar stm
  (** We provide two functions to create a transactional variable from common
      value: [tvar] is traditional toplevel declaration as those new* and
      create* functions seen in most other library, it is ensured to succeed;
      while [new_tvar] is a transactional declaration (as in Haskell) which may
      fail if the execution of the whole transaction it's bound in fails.

      We do not follow the idea on the relation between tvar allocation
      and exception from the original STM paper (last paragraph of section
      3.5), as we explicitly provide this two different declaration method:
      toplevel declaration [tvar] is ensured to be succeed; [new_tvar] is
      itself transactional, hence by no means should it be specially ensured to
      succeed always, if it's value is exposed after a fail transaction by
      exception etc, visiting it later will result in a Not_found exception,
      which exactly indicates what has happened: the [new_tvar] does not
      succeed.
  *)

val read_tvar: 'a tvar -> 'a stm
  (** Read value from a transactional variable, results in a transaction which
      can be further composed with other transactions through [bind] etc., or
      executed right away with [atom] etc. to get the final result
  *)

val write_tvar: 'a tvar -> 'a -> unit stm
  (** [write_tvar tv v] write value [v] to transactional variable [tv], results
      in a transaction whose type is [unit]. As [read_tvar], the result
      transaction is for composing or executing.

      Warning: do not operate the value of a transactional variable though other
      way exception write_tvar: such as producing the tvar from a mutable value
      or reference and secretly changing it in traditional way. First, it
      breaks the transactional semantics; second, in all possibility you won't
      be able to do that, as the value of tvar is isolated.
  *)

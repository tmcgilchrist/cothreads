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

(** A user-space STM library. This is the threads implementation.
*)


(* Lock mechanics *)

(* The commit and read is a classical reader-writer problem. We make our choice
   for the writer's preference, because the readings before writings are
   possibly turn out to be invalid in the future and cause unnecessary
   computation. Here we use single lock, a more elegant-in-theory solution
   would give each tvar a lock, however there are few chances it can speedup
   (esp. in the case of OCaml) and brings much more dangerous on deadlock and
   the complexity of solving them.
*)
let lock = Mutex.create ()

let writing = ref false
and readers = ref 0 (* the working readers *)
and writers = ref 0 (* the waiting writers *)

(* The conditions of exchange *)
let ok_to_read = Condition.create ()
and ok_to_write = Condition.create ()

(* Reader and Writer routines *)
let start_write () = 
  Mutex.lock lock;
  if !readers > 0 || !writing || !writers > 0 then
    begin 
      incr writers; 
      Condition.wait ok_to_write lock; 
      decr writers ;
    end;
  writing := true;
  Mutex.unlock lock
and end_write () =
  Mutex.lock lock;
  writing := false;
  if !writers > 0 then Condition.signal ok_to_write
  else Condition.broadcast ok_to_read;
  Mutex.unlock lock
and start_read () =
  Mutex.lock lock;
  while !writing || !writers > 0 do
    Condition.wait ok_to_read lock
  done;
  incr readers;
  Mutex.unlock lock
and end_read () =
  Mutex.lock lock;
  decr readers;
  if !readers = 0 then Condition.signal ok_to_write;
  Mutex.unlock lock

(* We should improve this to allow nested reader/writer *)
let reader f x =
  start_read (); 
  try let res = f x in end_read (); res 
  with e -> end_read (); raise e
and writer f x =
  start_write (); 
  try let res = f x in end_write (); res
  with e -> end_write (); raise e





(* Internal representation of tvar, log, env (bindings) and thread info *)

(* tid = thread_id * local_id *)
type tid = int * int and version = int and value = Obj.t * string

(* We need both Obj and Marshal. Marshal is used to isolate side-effect so
   that tvar won't change through other means except tvar operation, however it
   breaks the referencing relation between values, so the former
   is used to preseve the referencing relation between values (esp. tvars)
   which is then used by Weaktid module for GC. Just consider a tvar which has
   other tvars as its value, with Marshal back/forth, the reference realtion
   won't exist.
*)
let repr_of_val v = Obj.repr v, Marshal.to_string v [Marshal.Closures]
and val_of_repr (_,s) = Marshal.from_string s 0

(* Phantom type *)
type 'a tvar = unit -> tid


(* Tidtbl is used to define the environment (tvar * value/version bindings)
   and log (tvar * cur_value/rw_state etc. bindings). We must snapshot the
   whole environment (value bindings) before any atomic transactions, because
   tvars read on different points of a time series are actually inconsistent
   even if every updating is thread safe, besides we won't be able to know
   which tvars will be visited before the actual performing of each atomic
   transaction unit. Immutable data structure is used to save storage, otherwise
   we need a deep-persistent-copy. We use Map to get a immutable env with fast
   locating of tvar.
*)
module Tidtbl = Map.Make (struct type t = tid let compare = compare end)

type tvar_repr = 
    { version: version;
      value: value;
    }

(* env = tvar -> tvar_repr bindings *)
type env = tvar_repr Tidtbl.t

(* In a transaction log, we only care whether a tvar's first operation is read
   or write (useful for wait operation), and its current written value if being
   overwritten (useful for consequent read and commit). 
   A tvar never been accessed during the transaction should appear in the log 
*)
type log_item =
   { pre_version: version option; (* None = first op is write *)
     new_value: value option (* None = no write step, only read *)
    }

(* log = tvar -> log_item bindings *)
type log = log_item Tidtbl.t

(* Thrtbl is used to store thread local information, some of the design is
   esp. for the implementation of nested atom *)
module Thrtbl = Map.Make (struct type t = int let compare = compare end)

(* Thr_info is global, immutable and locks-depending, whereas different threads
   can work on its own thr_state independently and sequentially. We are safe to
   make these thread-local variables mutable and lock-free 
*)
type thr_info =
    { channel: unit Event.channel;
      wait_tid : tid list ;
      state: thr_state
    }
and thr_state =
    { mutable env: env;
      mutable log: log; (* When log is empty, env is free to get new env *)
      mutable tid_count : int;
      mutable layer: int (* For nested atom *)
    }

(* thr = thread_id -> thread_info bindings *)
type thr = thr_info Thrtbl.t


(* Global variables, should be always protected *)
let global_env = ref Tidtbl.empty
  (* let global_weaktid = Weaktid.create 200 *)
let global_thr = ref Thrtbl.empty


(* Some helper functions *)

let cur_thr_id () = Thread.id (Thread.self ())

(* Thread safe: !global_thr is atomic, and no other thread will create thread
   record instead current thread itself *)
let cur_thr () = Thrtbl.find (cur_thr_id ()) !global_thr

(* Some of the following operations should be defined inside reader/writer
   context. To be more flexible and imposable, they are not defined
   themselves with reader/writer locking mechanics inside. The rule of thumb is
   that only STM primitives that related to global state should be defined with
   locks, helper functions are not.
 *)
let new_thr () = 
  let thr_id = cur_thr_id () in
  let thr_info =
    { channel = Event.new_channel ();
      wait_tid = [];
      state = { env = !global_env;
                log = Tidtbl.empty;
                tid_count = 0;
                layer = 0
              }
    } in
  global_thr := Thrtbl.add thr_id thr_info !global_thr;
  thr_info


let global_rem = ref []

let remove_unreachable tid = 
  global_rem := tid :: !global_rem

(* valid_write take a log and env, produce log_item list option:  None means
   there are differences between the version on which the log is based and the
   version of env; Some l means no such differences and l is a list of
   modification (tid * value) to commit *) 
let valid_write log env = Tidtbl.fold
  (fun tid item res -> match res, item.pre_version with
   | None, _ -> None
   | _, Some v when v <> (Tidtbl.find tid env).version -> None
   | Some l, _ -> 
       match item.new_value with
       | None -> res
       | Some value -> Some ((tid, value) :: l)
  ) log (Some [])

(* valid_read take a log and env, produce log_item list option:  None means
   there are differences between the version on which the log is based and the
   version of env; Some l means no such differences and l is a list of tvar
   (tid) that have been read *)
let valid_read log env = Tidtbl.fold
  (fun tid item res -> match res, item.pre_version with
   | None, _ | _, None -> res
   | _, Some v when v <> (Tidtbl.find tid env).version -> None
   | Some l, Some v -> Some (tid :: l)
  ) log (Some [])

(* Only log and layer needs saving, env does not change during atom performing,
   tid_count should keep increasing with every attempting no matter success or
   fail 
*)
let save_state st = 
  {st with log = st.log; layer = st.layer} 
let restore_state st st_bak =
  st.log <- st_bak.log; st.layer <- st_bak.layer




(* Transaction semantics *)

type 'a stm = thr_state -> 'a

let return v = fun state -> v

let bind t f = fun state -> f (t state) state

let ( >>= ) = bind
let ( >> ) s1 s2 = s1 >>= (fun _ -> s2)





(* Tvar semantics *)

(* Global operation, locks required *)
let tvar v = 
  let thr_state = 
    try (reader cur_thr ()).state
    (* we are safe to separate test-read and write operations here, as no other
       threads will create thread record between them instead of the current
       thread itself *)
    with Not_found -> (writer new_thr ()).state in
  thr_state.tid_count <- succ thr_state.tid_count;
  let new_tid = (cur_thr_id (), thr_state.tid_count) in
  let _ = Gc.finalise (remove_unreachable) new_tid in  
  let new_tvar = fun () -> new_tid in
  let new_repr =
    { version = 0;
      value = repr_of_val v;
    } in
  thr_state.env <- Tidtbl.add new_tid new_repr thr_state.env;
  writer 
    (fun () ->
       global_env := Tidtbl.add new_tid new_repr !global_env
    ) ();
  new_tvar

let new_tvar v = fun state ->
  state.tid_count <- succ state.tid_count;
  let new_tid = (cur_thr_id (), state.tid_count) in
  let _ = Gc.finalise (remove_unreachable) new_tid in
  let new_tvar = fun () -> new_tid in
  let new_log_item =
    { pre_version = None;
      new_value = Some (repr_of_val v)
    } in
  state.log <- Tidtbl.add new_tid new_log_item state.log;
  new_tvar

let read_tvar tv = fun state -> 
  try 
    let val_repr = 
      match (Tidtbl.find (tv ()) state.log).new_value with
      | None -> (Tidtbl.find (tv ()) state.env).value
      | Some v -> v in
    val_of_repr val_repr
  with Not_found -> 
    let tv_repr = Tidtbl.find (tv ()) state.env in
    let new_item = {pre_version = Some tv_repr.version; new_value = None} in
    state.log <- Tidtbl.add (tv ()) new_item state.log;
    val_of_repr tv_repr.value

let write_tvar tv v = fun state ->
  let log_item = 
    try 
      { (Tidtbl.find (tv ()) state.log) with new_value = Some (repr_of_val v) }
    with Not_found -> 
      { pre_version = None; 
        new_value = Some (repr_of_val v)
      } in
  state.log <- Tidtbl.add (tv ()) log_item state.log

(* We use synchronized event here in order to be able to leave critical section
   by beginning a continuous waiting. Asynchronized event (Event.poll) won't be
   able to ensure the seamless connection of the leaving and
   beginning. Condition.wait seems natural, but won't be able to allow us to
   customize the locking mechanics as we do with the reader/writer solution.

   Global operation, lock required.
*)
let wait state  =
  let e = writer
    (fun () ->
       let env = !global_env in
       match valid_read state.log env with
       | None | Some [] -> Event.always ()
       | Some l -> 
           let new_thr_info = {(cur_thr ()) with wait_tid = l} in
           global_thr := Thrtbl.add (cur_thr_id ()) new_thr_info !global_thr;
           Event.receive new_thr_info.channel) () in
  Event.sync e

let commit = writer 
  (fun log -> 
     global_env := List.fold_left (fun e x -> if Tidtbl.mem x e then
                                     Tidtbl.remove x e else e) !global_env !global_rem;
     global_rem := [];
     let env = !global_env in
     let thr = !global_thr in
     match valid_write log env with
     | None -> false 
     | Some [] -> true
     | Some l -> 
         let new_env, new_thr = List.fold_left 
           (fun (old_env,old_thr) (tid, value) ->
              let version =
                try succ (Tidtbl.find tid old_env).version
                with Not_found -> 0 in
              Tidtbl.add tid {version = version; value = value} old_env, 
              Thrtbl.fold
                (fun id item thr -> 
                   if List.mem tid item.wait_tid then
                     (Event.sync (Event.send item.channel ()); 
                      Thrtbl.add id {item with wait_tid = []} thr)
                   else thr)
                old_thr old_thr
           ) (env, thr) l in
         global_env := new_env; global_thr := new_thr;
         true
  ) 


exception Abort
exception Retry of bool (* whether wait or not *)

let abort = fun _ -> raise Abort
let retry = fun _ -> raise (Retry true)
let retry_now = fun _ ->  raise (Retry false)

let catch t f = fun state -> 
  let st_bak = save_state state in
  try t state with 
  | Retry _ | Abort as e -> raise e
  | e -> restore_state state st_bak; f e state

let or_else t1 t2 = fun state ->
  let state_bak = save_state state in
  try t1 state
  with Abort | Retry _ as r1 ->
    let state_bak_1 = save_state state in
    restore_state state state_bak;
    try t2 state
    with Abort | Retry _ as r2 ->
      match r1, r2 with
      | Abort, Abort -> restore_state state state_bak; raise Abort
      | Retry b, Abort -> restore_state state state_bak_1; raise (Retry b)
      | Abort, Retry b -> raise (Retry b)
      | Retry b1, Retry b2 ->
          (* Mix two logs are unavoidably dirty, fortunately we only cares
             about the reading records *)
          state.log <- Tidtbl.fold 
            (fun tid log_item log -> 
               match log_item.pre_version with
               | None -> log
               | Some v -> 
                   try (match (Tidtbl.find tid log).pre_version with
                        | Some _ -> log
                        | None -> raise Not_found)
                   with Not_found -> Tidtbl.add tid log_item log)
            state_bak_1.log state.log;
          raise (Retry (b1 && b2))
      | _, _ -> assert false

let rec atom_once t =
  let state = 
    try
      let st = (cur_thr ()).state in
      if st.layer = 0 then (st.env <- !global_env; st.log <- Tidtbl.empty); 
      st
    with Not_found -> (writer new_thr ()).state in
  (* Really enter by succ layer *)
  state.layer <- succ state.layer;
  try 
    let v = t state in
    (* Quick to upper layer *)
    state.layer <- pred state.layer; 
    if state.layer > 0 || commit state.log then Some v else None
  with e ->
    state.layer <- pred state.layer;
    match state.layer, e with 
    | 0, Retry b -> if b then wait state; atom_once t 
    | 0, Abort -> None
    | _, _ -> raise e

let rec atom t = match atom_once t with None -> atom t | Some v -> v

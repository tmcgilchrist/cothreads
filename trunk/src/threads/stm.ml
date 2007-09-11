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


(** Lock mechanics *)

(* The commit and read is a classical reader-writer problem. We make our choice
   for the writer's preference, because the readings before writings are
   possibly turn out to be invalid in the future and cause unnecessary
   computation. Here we use single lock, a more elegant-in-theory solution
   would give each tvar a lock, however there are very few chances it can
   speedup (esp. in the presense of OCaml'a master lock) and brings much more
   dangerous on deadlock and the complexity of solving them.
*)
let lock = Mutex.create ()

let writing = ref None
and readers = ref 0 (* the working readers *)
and writers = ref 0 (* the waiting writers *)

(* The conditions of exchange *)
let ok_to_read = Condition.create ()
and ok_to_write = Condition.create ()

(* Reader and Writer routines *)
let start_write () = 
  let my_id = Thread.id (Thread.self ()) in
  Mutex.lock lock;
  (match !writing with
   | Some (id, num) when id = my_id -> writing := Some (id, num + 1)
   | None when !readers = 0 && !writers = 0 -> writing := Some (my_id, 1)
   | _ ->
       incr writers; 
       Condition.wait ok_to_write lock; 
       decr writers ;
       writing := Some (my_id, 1));
  Mutex.unlock lock
and end_write () =
  Mutex.lock lock;
  match !writing with 
  | None -> assert false 
  | Some (id, n) ->
      if n = 1 then 
        (writing := None;
         if !writers > 0 then Condition.signal ok_to_write
         else Condition.broadcast ok_to_read)
      else writing := Some (id, pred n);
      Mutex.unlock lock
and start_read () =
  Mutex.lock lock;
  while !writing <> None || !writers > 0 do
    Condition.wait ok_to_read lock
  done;
  incr readers;
  Mutex.unlock lock
and end_read () =
  Mutex.lock lock;
  decr readers;
  if !readers = 0 then Condition.signal ok_to_write;
  Mutex.unlock lock

let reader f x =
  start_read (); 
  try let res = f x in end_read (); res 
  with e -> end_read (); raise e
and writer f x =
  start_write (); 
  try let res = f x in end_write (); res
  with e -> end_write (); raise e


(** Internal representation of tvar, log, env (bindings) and thread info *)

(* Magic number *)
let stm_magic = 20072007

(* tid = stm_magic * thread_id * local_id, used as pure identification; tvid
   not only has tid inside, but also keep refrence (for GC) between each
   other. In such sense, tvid must carefully preserve physical
   linking/equivelence  property, while tid doesn't.
*)
type tid = int * int * int and tvid = {tid: tid; mutable dirty: tvid list}

(* We need both Obj and Marshal. Marshal is used to isolate side-effect so that
   tvar won't change through other means except tvar operation, however it
   breaks the referencing relation between values, so the former is used to
   preseve the referencing relation between values (esp. tvars) which is then
   used by Weaktidtbl module for GC. Just consider a tvar which has other tvars
   as its value, with Marshal back/forth, the reference realtion won't exist. 
*)
type version = int and value = Obj.t and record = string

let rec_of_var v = Marshal.to_string v [Marshal.Closures]
let var_of_rec s = Marshal.from_string s 0
let rec_of_val o = Marshal.to_string (Obj.obj o) [Marshal.Closures]
let val_of_rec s = Obj.repr (Marshal.from_string s 0)
let val_of_var v = Obj.repr v
let var_of_val o = Obj.obj o

let tvid_eq {tid = tid1} {tid = tid2} = tid1 = tid2
let tvid_comp {tid = tid1} {tid = tid2} = compare tid1 tid2
let tvid_hash {tid = tid} = Hashtbl.hash tid
let is_tvid o = 
  Obj.tag o = 0 && let o' = Obj.field o 0 in 
  Obj.tag o' = 0 && Obj.field o' 0 = Obj.repr stm_magic
let tvid_obj_eq o1 o2 = 
  is_tvid o1 && is_tvid o2 && Obj.field o1 0 = Obj.field o2 0

let filter_dirty v fold comb l base = fold 
  (fun tvid ll -> 
     if Libext.obj_refed_by tvid_obj_eq tvid v then comb tvid ll else ll
  ) l base
let filter_subst v iter tbl =
  let r = ref v in
  iter (fun tvid -> Libext.obj_subst tvid_obj_eq (tvid,tvid) r) tbl;
  !r


(* Tidtbl is used to define the environment (tid * value/version bindings) and
   log (tid * cur_value/rw_state etc. bindings). We must snapshot the whole
   environment (value bindings) before any atomic transactions, because tvars
   read on different points of a time series are actually inconsistent even if
   every updating is thread safe, besides we won't be able to know which tvars
   will be visited before the actual performing of each atomic transaction
   unit. Immutable data structure is used to save storage, otherwise we need a
   deep-persistent-copy. We use Map to get a immutable env.
*)
module Tidtbl = Map.Make 
  (struct type t = tid let compare = compare end)

module Tvidtbl = Map.Make
  (struct type t = tvid let compare = tvid_comp end)

module Weaktidtbl = Weak.Make 
  (struct type t = tvid let equal = tvid_eq let hash = tvid_hash end)

module Thrtbl = Map.Make 
  (struct type t = int let compare = compare end)

(* env = tid -> tvar_repr bindings *)
type env = tvar_repr Tidtbl.t
and tvar_repr = { version: version; record: record }

(* In a transaction log, we only care whether a tvar's first operation is read
   or write (useful for wait operation), and its current written value if being
   overwritten (useful for consequent read and commit). 
   log = tvid -> log_item bindings 
*)
type log = log_item Tvidtbl.t
and log_item =
   { pre_version: version option; (* None = first op is write *)
     new_value: value option  (* None = no write step, only read *)
   }

(* Thr_info is global, immutable and locks-depending, whereas different threads
   can work on its own thr_state independently and sequentially. We are safe to
   make these thread-local variables mutable and lock-free.
   thr = thread_id -> thread_info bindings *)
type thr = thr_info Thrtbl.t
and thr_info =
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

(* Global variables information, should always be protected *)
let global_env = ref Tidtbl.empty

(* Global thread information *)
let global_thr = ref Thrtbl.empty

(* Global weak tid information, should always be proteced *)
let global_weaktid = Weaktidtbl.create 29

(* Global to_remove list for env *)
let global_remove = ref []


(* Some helper functions follows *)


(* Thread safe: !global_thr is atomic, and no other thread will create thread
   record instead current thread itself *)
let cur_thr_id () = Thread.id (Thread.self ())
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
                log = Tvidtbl.empty;
                tid_count = 0;
                layer = 0
              }
    } in
  global_thr := Thrtbl.add thr_id thr_info !global_thr;
  thr_info

(* Hopefully atomic *)
let clean_tid tvid = global_remove := tvid.tid :: !global_remove

let clean_env () =
  let to_remove = !global_remove in global_remove := [];
  global_env := List.fold_right Tidtbl.remove to_remove !global_env

(* valid_write take a log and env, produce (tvid list * log_item) list option:
   None means there are some contradictions between the version on which the
   log is based and the version of env; Some (r,w)  means no contradictions and
   r a list of tvid been read and w is a list of tvid * value been written.
*) 
let valid_write log env = Tvidtbl.fold
  (fun tvid item res -> match res, item.pre_version, item.new_value with
   | None, _, _ -> None
   | Some (r,w), None, Some value -> Some (r, (tvid, value)::w)
   | Some _, None, None -> assert false
   | Some (r,w), Some v, value ->
       if v <> (Tidtbl.find tvid.tid env).version then None
       else Some (tvid :: r, match value with None -> w | Some value -> (tvid, value) :: w)
  ) log (Some ([],[]))

(* valid_read take a log and env, produce tvid list option: None means there
   are contradictions between log-based version and the version of env; Some l
   means no contradictions and l is a list of tvid been read 
*)
let valid_read log env = Tvidtbl.fold
  (fun tvid item res -> match res, item.pre_version with
   | None, _ | _, None -> res
   | _, Some v when v <> (Tidtbl.find tvid.tid env).version -> None
   | Some l, Some v -> Some (tvid :: l)
  ) log (Some [])

(* When saving/restoring state, Only log and layer needs saving, env does not
   change during atom performing, tid_count should keep increasing with every
   attempting no matter success or fail. 
*)
let save_state st = {st with log = st.log; layer = st.layer} 
let restore_state st st_bak = st.log <- st_bak.log; st.layer <- st_bak.layer

(* Be sure to reset as soon as possible, otherwise there could be memory leak due
   to unnecessary residu tvar, such as the mcast example *)
let reset_state state = state.env <- !global_env; state.log <- Tvidtbl.empty
let clean_state state = state.env <- Tidtbl.empty; state.log <- Tvidtbl.empty



(** Transaction semantics *)

(* Phantom type *)
type 'a tvar = tvid

type 'a stm = thr_state -> 'a
let return v = fun state -> v
let bind t f = fun state -> f (t state) state
let ( >>= ) = bind
let ( >> ) s1 s2 = s1 >>= (fun _ -> s2)

(* Non-transactional declaration of new tvar, ensure to success *)
let tvar v = 
  let thr_state = 
    try (reader cur_thr ()).state
    (* we are safe to separate test-read and write operations here, as no other
       threads will create the record for the current thread except itself *)
    with Not_found -> (writer new_thr ()).state in
  thr_state.tid_count <- succ thr_state.tid_count;
  let new_tid = (stm_magic, cur_thr_id (), thr_state.tid_count) in
  let new_repr = {version = 0; record = rec_of_var v} in
  writer 
    (fun () ->
       let dirty = 
         filter_dirty v Weaktidtbl.fold (fun x l -> x::l) global_weaktid [] in
       let new_tvid = {tid = new_tid; dirty = dirty} in
       let _ = Gc.finalise clean_tid new_tvid in
       thr_state.env <- Tidtbl.add new_tid new_repr thr_state.env;
       global_env := Tidtbl.add new_tid new_repr !global_env;
       Weaktidtbl.add global_weaktid new_tvid;
       new_tvid
    ) ()

(* Transactional declaration of new tvar *)
let new_tvar v = fun state ->
  state.tid_count <- succ state.tid_count;
  let new_tid = (stm_magic, cur_thr_id (), state.tid_count) in
  let new_tvid = {tid =new_tid; dirty = []} in
  let new_log_item =
    { pre_version = None;
      new_value = Some (val_of_var v)
    } in
  state.log <- Tvidtbl.add new_tvid new_log_item state.log;
  new_tvid

let read_tvar tv = fun state -> 
  try 
    match (Tvidtbl.find tv state.log).new_value with
    | None -> var_of_rec (Tidtbl.find tv.tid state.env).record
    | Some v -> var_of_val v
  with Not_found -> 
    let tv_repr = Tidtbl.find tv.tid state.env in
    let new_item = {pre_version = Some tv_repr.version; new_value = None} in
    state.log <- Tvidtbl.add tv new_item state.log;
    var_of_rec tv_repr.record

let write_tvar tv v = fun state ->
  let new_value = Some (val_of_var v) in
  let log_item = 
    try {(Tvidtbl.find tv state.log) with new_value = new_value}
    with Not_found -> {pre_version = None; new_value = new_value} in
  state.log <- Tvidtbl.add tv log_item state.log

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
       match valid_read state.log !global_env with
       | None | Some [] -> Event.always ()
       | Some l -> 
           let new_thr_info = 
             {(cur_thr ()) with wait_tid = List.map (fun x -> x.tid) l} in
           global_thr := Thrtbl.add (cur_thr_id ()) new_thr_info !global_thr;
           Event.receive new_thr_info.channel) () in
  Event.sync e

let commit log v = writer 
  (fun (log, v) -> 
     match valid_write log !global_env with
     | None -> None
     | Some (r, w) -> 
         let suspecious_r = List.fold_left 
           (fun l tvid -> (Weaktidtbl.find global_weaktid tvid).dirty @ l) 
           r r in
         let suspecious_w, old_dirty = List.fold_left
           (fun (sw,od) (tvid,_) ->
              try sw, ((Weaktidtbl.find global_weaktid tvid).dirty @ od)
              with Not_found -> 
                Gc.finalise clean_tid tvid;
                Weaktidtbl.add global_weaktid tvid; 
                (tvid::sw, od)) 
           ([], []) w in
         let suspecious = suspecious_w @ suspecious_r in
         List.iter 
           (fun (tvid, value) ->
              let dirty = 
                filter_dirty value List.fold_right (fun x l -> x::l) suspecious [] in
              (Weaktidtbl.find global_weaktid tvid).dirty <- dirty;
              (global_env := 
                 let repr = 
                   { record = rec_of_val value;
                     version = 
                       try (Tidtbl.find tvid.tid !global_env).version + 1
                       with Not_found -> 0
                   } in
                 Tidtbl.add tvid.tid repr !global_env);
              (global_thr := Thrtbl.fold
                 (fun id item thr -> 
                    if List.mem tvid.tid item.wait_tid then
                      (Event.sync (Event.send item.channel ()); 
                       Thrtbl.add id {item with wait_tid = []} thr)
                    else thr)
                 !global_thr !global_thr)
           ) w;
         clean_env ();
         Some (filter_subst v Weaktidtbl.iter global_weaktid)
  ) (log, v)


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
          state.log <- Tvidtbl.fold 
            (fun tvid log_item log -> 
               match log_item.pre_version with
               | None -> log
               | Some v -> 
                   try (match (Tvidtbl.find tvid log).pre_version with
                        | Some _ -> log
                        | None -> raise Not_found)
                   with Not_found -> Tvidtbl.add tvid log_item log)
            state_bak_1.log state.log;
          raise (Retry (b1 && b2))
      | _, _ -> assert false

let rec atom_once t =
  let state = try
    let st = (cur_thr ()).state in
    if st.layer = 0 then reset_state st;
    st
  with Not_found -> (writer new_thr ()).state in
  (* Really enter by succ layer *)
  state.layer <- succ state.layer;
  try
    let v = t state in
    (* Quit to upper layer *)
    state.layer <- pred state.layer;
    if state.layer > 0 then Some v
    else
      let log = state.log in
      let _ = clean_state state in
      commit log v
  with e ->
    state.layer <- pred state.layer;
    match state.layer, e with 
    | 0, Retry b -> if b then wait state; atom_once t 
    | 0, Abort -> clean_state state; None
    | _, _ -> raise e

let rec atom t = match atom_once t with None -> atom t | Some v -> v

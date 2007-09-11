open Coordinator
open Libext

let stm_magic = "STM2007MTS"

type tvid = string * int * int and version = int and value = Obj.t

module TvMap = Map_Make (struct type t = tvid let compare = Pervasives.compare end)
module TvSet = Set.Make (struct type t = tvid let compare = Pervasives.compare end)

type tv_repr = {version: version; value: value; ref_to: TvSet.t}

(* New: pre_version(value) = None
   First Read: pre_version(value) = Some _
   First Write: pre_version = Some _, pre_value = None *)
type tv_log = {pre_version: version option; pre_value: value option;
               mutable cur_value: value option}

type commit_log =
    { read_log: version TvMap.t; write_log: tv_repr TvMap.t; dirty_log: TvSet.t }

type stm_msg =
  [ `Tvar of tvid * tv_repr * thread * bool portal
  | `Wait of version TvMap.t * thread * bool portal
  | `Atom of thread * tv_repr TvMap.patch portal
  | `Commit of commit_log * thread * bool portal
  ]

let stm_portal : stm_msg portal = create_portal ()

let repr_eq {version=v1} {version=v2} = v1 = v2
let var_of_val v = Obj.obj v and val_of_var v = Obj.repr v

type 'a tvar = tvid

type thr_state =
    { mutable env: tv_repr TvMap.t; mutable log: tv_log TvMap.t;
      mutable tvid_count: int; mutable layer: int; mutable dirty: TvSet.t; }

let state = 
  { env = TvMap.empty; log = TvMap.empty; tvid_count = 0; 
    layer = 0; dirty = TvSet.empty }

let state_reset diff =
  assert (state.layer = 0);
  state.env <- TvMap.patch_left repr_eq state.env diff; 
  state.log <- TvMap.empty


(* Shallow copy, only work for data structure like tvid *)
let copy (x:tvid) : tvid = Obj.obj (Obj.dup (Obj.repr x))
let tvmap_add k = TvMap.add (copy k)
let tvset_add k = TvSet.add (copy k)
let finaliser vl = state.dirty <- TvSet.remove (var_of_val vl) state.dirty
let suspicious () =
  TvMap.fold (fun tv log set -> match log with
              | {pre_version = Some _; pre_value = Some _} -> 
                  TvSet.union (TvMap.find tv state.env).ref_to set
              | {pre_version = None} -> TvSet.add tv set
              | _ -> set
             ) state.log state.dirty


type 'a stm = unit -> 'a
let return v = fun () -> v
let bind t f = fun () -> f (t ()) ()
let ( >>= ) = bind
let ( >> ) s1 s2 = s1 >>= fun _ -> s2

let reference v = TvSet.filter (fun x -> obj_refed_by (=) x v)

let tvar v =
  if (not !inited) then init ();
  let self_t = self () in
  let self_id = id self_t in
  state.tvid_count <- succ state.tvid_count;
  let new_tvid = (stm_magic, self_id, state.tvid_count) in
  let ref_to = reference v (suspicious ()) in
  let new_repr = {version=0; value=val_of_var v; ref_to = ref_to} in
  let b = demand_portal 
    (fun p -> `Tvar (new_tvid, new_repr, self_t, p)) stm_portal in
  assert b;
  state.env <- tvmap_add new_tvid new_repr state.env;
  state.dirty <- tvset_add new_tvid state.dirty;
  Gc.finalise finaliser (val_of_var new_tvid);
  new_tvid

let new_tvar v = fun () ->
  let self_id = id (self ()) in
  state.tvid_count <- succ state.tvid_count;
  let new_tvid = (stm_magic, self_id, state.tvid_count) in
  let new_log = 
    { pre_version = None; pre_value = None; 
      cur_value = Some (val_of_var v) } in
  state.log <- tvmap_add new_tvid new_log state.log;
  new_tvid

let read_tvar tv = fun () ->
  let value = try
    let log = TvMap.find tv state.log in
    match log.cur_value, log.pre_value with
    | Some v, _ | _, Some v -> v | _ -> assert false
  with Not_found -> 
    let repr = TvMap.find tv state.env in
    state.log <- tvmap_add tv 
      { pre_version = Some repr.version; 
        pre_value = Some repr.value; cur_value = None } 
      state.log;
    repr.value in
  var_of_val value

let write_tvar tv v = fun () ->
  try
    let log = TvMap.find tv state.log in
    log.cur_value <- Some (val_of_var v)
  with Not_found ->
    let repr = TvMap.find tv state.env in
    state.log <- TvMap.add tv 
      { pre_version = Some repr.version; pre_value = None;
        cur_value = Some (val_of_var v) }
      state.log

let wait = fun () ->
  let wait_tv = TvMap.fold 
    (fun tv log map -> match log with
     | {pre_version = Some v; pre_value = Some _} -> TvMap.add tv v map
     | _ -> map)
    state.log TvMap.empty in
  assert (demand_portal (fun p -> `Wait (wait_tv, self (), p)) stm_portal)

exception Abort
exception Retry of bool (* whether wait or not *)
let abort = fun () -> raise Abort
let retry = fun () ->  raise (Retry true)
let retry_now = fun () -> raise (Retry false)
let save_state st = {st with layer = st.layer} (* actually copy everything *)
let restore_state st st_bak = st.log <- st_bak.log; st.layer <- st_bak.layer

let catch t f = fun () ->
  let state_bak = save_state state in
  try t () with
  | Retry _ | Abort as e -> raise e
  | e -> restore_state state state_bak; f e ()

let or_else t1 t2 = fun () ->
  let state_bak = save_state state in
  try t1 () with (Abort | Retry _) as e1 ->
    let state_bak_1 = save_state state in
    restore_state state state_bak;
    try t2 () with (Abort | Retry _) as e2 ->
      match e1, e2 with
      | Abort, Abort -> restore_state state state_bak; raise Abort
      | Retry b, Abort -> restore_state state state_bak_1; raise (Retry b)
      | Abort, Retry b -> raise (Retry b)
      | Retry b1, Retry b2 ->
          let comb_log = TvMap.merge
            (fun k v1 v2 tbl -> match v1.pre_value,v2.pre_value with
             | None, Some _ -> TvMap.add k v2 tbl | _,_ -> tbl)
            state.log state_bak_1.log in
          restore_state state state_bak;
          state.log <- comb_log;
          raise (Retry (b1 && b2))
      | _, _ -> assert false

let dirtirise v susp = 
  let mark tv = 
    let val_tv = val_of_var tv in
    obj_iter 
      (fun o -> 
         if o = val_tv then 
           (state.dirty <- TvSet.add tv state.dirty; Gc.finalise finaliser o)
      ) v in 
  TvSet.iter mark susp


(* Compute locally to save the effort of coordinator *)
let commit_log susp =
  let read,write = TvMap.fold 
    (fun tv {pre_version=pver; pre_value=pval; cur_value=cval} (r,w) -> 
       let w = match cval with
         | Some v -> 
             let repr = 
               { version= 0; (* tmp_value, to be changed when commit *) 
                 value=v; ref_to=reference v susp } in
             TvMap.add tv repr w
         | None -> w in
       let r = match pval, pver with
         | Some _, Some ver -> TvMap.add tv ver r
         | _ -> r in
       (r, w)
    ) state.log (TvMap.empty, TvMap.empty) in
  {read_log = read; write_log = write; dirty_log = state.dirty}


let commit v : bool = 
  let susp = suspicious () in
  let _ = dirtirise v susp in
  let clog = commit_log susp in
  demand_portal (fun p -> `Commit (clog, self (), p)) stm_portal


let rec atom_once t =
  if (not !inited) then init ();
  (if state.layer = 0 then 
     let diff = demand_portal (fun p -> `Atom (self (), p)) stm_portal in
     state_reset diff);
  state.layer <- succ state.layer;
  try
    let v = t () in
    state.layer <- pred state.layer;
    if state.layer > 0 || commit v then Some v else None
  with e ->
    state.layer <- pred state.layer;
    match state.layer, e with
    | 0, Retry b -> if b then wait (); atom_once t
    | 0, Abort -> None
    | _,_ -> raise e

let rec atom t = match atom_once t with None -> atom t | Some v -> v


(* Root Service *)

type tv_rec = 
    { mutable ref_by_tv: TvSet.t; 
      mutable ref_by_thr: ThreadSet.t;
      mutable tv_wait: bool portal option ref list }

type thr_rec = 
    { mutable thr_env: tv_repr TvMap.t; 
      mutable tv_dirty: TvSet.t }

type stm_root = 
    { mutable root_env: tv_repr TvMap.t;
      mutable root_rec: tv_rec TvMap.t;
      mutable root_thr: thr_rec ThreadMap.t;
    }

let root = {root_env = TvMap.empty; root_rec = TvMap.empty; root_thr = ThreadMap.empty}

let empty_tv_rec () = 
  { ref_by_tv = TvSet.empty; ref_by_thr = ThreadSet.empty; tv_wait = []}
let empty_thr_rec () =
  { thr_env = root.root_env; tv_dirty = TvSet.empty }


(* Primitive Tvar service *)
let tvar_handle tv repr thr p = 
  let tv_rec = empty_tv_rec () in
  tv_rec.ref_by_thr <- ThreadSet.add thr tv_rec.ref_by_thr;
  root.root_env <- TvMap.add tv repr root.root_env;
  root.root_rec <- TvMap.add tv tv_rec root.root_rec;
  let thr_rec = ThreadMap.find thr root.root_thr in
  thr_rec.thr_env <- TvMap.add tv repr thr_rec.thr_env;
  thr_rec.tv_dirty <- TvSet.add tv thr_rec.tv_dirty;
  write_portal true p

(* Primitive Wait service *)
let wait_handle wait_tv thr p = 
  let answer_port = ref None in
  let mark tv version =
    let tv_repr = TvMap.find tv root.root_env in
    if tv_repr.version > version then raise Break else 
      let reco = TvMap.find tv root.root_rec in
      reco.tv_wait <- answer_port :: reco.tv_wait in
  try
    TvMap.iter mark wait_tv;
    answer_port := Some p
  with Break -> write_portal true p

(* Primitve Atom service *)
let atom_handle thr p = 
  let thr_rec = ThreadMap.find thr root.root_thr in
  let diff = TvMap.diff repr_eq thr_rec.thr_env root.root_env in
  thr_rec.thr_env <- root.root_env;
  write_portal diff p

let opr_ref_by_thr op tv =
  let tv_rec = TvMap.find tv root.root_rec in
  tv_rec.ref_by_thr <- op tv_rec.ref_by_thr
let opr_ref_by_tv op tv =
  let tv_rec = TvMap.find tv root.root_rec in
  tv_rec.ref_by_tv <- op tv_rec.ref_by_tv

(* Primitve Commit service *)
let commit_handle {read_log=rl; write_log=wl; dirty_log=dl} thr p =
  let conflict = try
    TvMap.iter 
      (fun tv ver -> 
         if (TvMap.find tv root.root_env).version <> ver then raise Break)
      rl;
    false
  with Break | Not_found -> true in
  if conflict then write_portal false p else begin
    (* References decreasing set *)
    let ref_dec_set = ref TvSet.empty in
    (* we must first update the whole root_env to its final state before we
       begin to test dirty sets relation, otherwise there will be
       inconsistence *)
    let _ = TvMap.iter
      (fun tv repr ->
         try
           let old_repr = TvMap.find tv root.root_env in
           let new_repr = {repr with version = old_repr.version + 1} in
           TvSet.iter (opr_ref_by_tv (TvSet.remove tv)) old_repr.ref_to;
           ref_dec_set := TvSet.union !ref_dec_set old_repr.ref_to;
           (* we can not handle new references to other tv at this moment,
              because not all tv has been commited in *)
           root.root_env <- TvMap.add tv new_repr root.root_env;
           (* reactive waiting thread because of the value change *)
           let tv_rec = TvMap.find tv root.root_rec in
           List.iter (fun w -> match !w with 
                      | Some p -> (write_portal true p; w := None)
                      | None -> ()
                     ) tv_rec.tv_wait;
           tv_rec.tv_wait <- []
         with Not_found -> (* only reason: new tvar; collected: impossible *)
           root.root_env <- TvMap.add tv repr root.root_env;
           (* create record for new tvar now, in case of dangling points when
              updating reference *)
           root.root_rec <- TvMap.add tv (empty_tv_rec ()) root.root_rec;
      ) wl in
    (* handle new reference now *)
    let _ = TvMap.iter
      (fun tv repr -> 
         TvSet.iter (opr_ref_by_tv (TvSet.add tv)) repr.ref_to;
         ref_dec_set := TvSet.diff !ref_dec_set repr.ref_to
      ) wl in
    (* We update thr_rec in the next step *)
    let _ = 
      let thr_rec = ThreadMap.find thr root.root_thr in
      let to_remove = TvSet.diff thr_rec.tv_dirty dl in
      let to_add = TvSet.diff dl thr_rec.tv_dirty in
      TvSet.iter (opr_ref_by_thr (ThreadSet.remove thr)) to_remove;
      ref_dec_set := TvSet.union !ref_dec_set to_remove;
      TvSet.iter (opr_ref_by_thr (ThreadSet.add thr)) to_add;
      ref_dec_set := TvSet.diff !ref_dec_set to_add;
      thr_rec.tv_dirty <- dl in
    (* Finally doing house mantinance: GC *)
    let _ = 
      let rec gc tv_set = 
        let tv = TvSet.max_elt tv_set in
        let tv_rest = TvSet.remove tv tv_set in
        let tv_rec = TvMap.find tv root.root_rec in
        if TvSet.is_empty tv_rec.ref_by_tv && 
          ThreadSet.is_empty tv_rec.ref_by_thr 
        then
          let ref_to = (TvMap.find tv root.root_env).ref_to in
          root.root_rec <- TvMap.remove tv root.root_rec;
          root.root_env <- TvMap.remove tv root.root_env;
          TvSet.iter (opr_ref_by_tv (TvSet.remove tv)) ref_to;
          gc (TvSet.union ref_to tv_rest)
        else gc tv_rest in
      try gc !ref_dec_set with Not_found -> () in
    (* For now, we don't update the env record of thread, to make it agree with
       old_env of client; only when next atom requirement, we diff the current
       root_env with this version, update this version,  and send out patchs
    *)
    write_portal true p
  end

let stm_extend_handle : root_msg -> unit = function
  | `Create (t', t, _) ->
      (try 
        let fat_thr = ThreadMap.find t' root.root_thr in
        let son_thr = {fat_thr with tv_dirty = fat_thr.tv_dirty} in
        root.root_thr <- ThreadMap.add t son_thr root.root_thr;
        TvSet.iter (opr_ref_by_thr (ThreadSet.add t)) son_thr.tv_dirty;
       with Not_found -> (* The first one *)
         root.root_thr <- ThreadMap.add t (empty_thr_rec ()) root.root_thr)
  | `Delete (t, _) ->
      let thr_rec = ThreadMap.find t root.root_thr in
      TvSet.iter (opr_ref_by_thr (ThreadSet.remove t)) thr_rec.tv_dirty;
      root.root_thr <- ThreadMap.remove t root.root_thr
  | _ -> ()

let stm_handle : stm_msg -> unit = function
  | `Tvar (new_tvid, new_repr, self_t, p) -> 
      tvar_handle new_tvid new_repr self_t p
  | `Wait (touch, thr, p) -> wait_handle touch thr p
  | `Atom (thr, p) -> atom_handle thr p
  | `Commit (clog, thr, p) -> commit_handle clog thr p

let _ = new_serv root_portal stm_extend_handle
let _ = new_serv stm_portal stm_handle

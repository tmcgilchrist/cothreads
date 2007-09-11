exception Break
exception NoImplementationYet
let noimplementation x = raise NoImplementationYet

let list_find_split =
  let rec find_rec test acc = function
    | [] -> raise Not_found
    | h :: t -> if test h then (acc, h, t) else find_rec test (h::acc) t in
  fun test l -> find_rec test [] l

let rec list_find_app f = function
  | [] -> None
  | h::t -> match f h with Some _ as v -> v | None -> list_find_app f t

let bit_chop_to_n n x =
  let capability = 1 lsl n - 1 in
  x land capability


module Map_Make (Ord:Map.OrderedType) : sig
  include Map.S
  type 'a patch = (key * ('a option * 'a option)) list
  val diff: ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a patch
  val patch_left: ('a -> 'a -> bool) -> 'a t -> 'a patch -> 'a t
  val patch_right: ('a -> 'a -> bool) -> 'a patch -> 'a t -> 'a t
  val merge: (key -> 'a -> 'a -> 'a t -> 'a t) -> 'a t -> 'a t -> 'a t
end with type key = Ord.t = struct
  include Map.Make (Ord)
  type 'a patch = (key * ('a option * 'a option)) list
  let to_list t = fold (fun k v l -> (k,v)::l) t []  (* decrease order *)
  let diff eq t1 t2 =
    let lt1 = to_list t1 and lt2 = to_list t2 in
    let rec diff_aux eq accu = function
      | [], l -> List.fold_left (fun a (k, v) -> (k, (None, Some v))::a) accu l
      | l, [] -> List.fold_left (fun a (k, v) -> (k, (Some v, None))::a) accu l
      | (((k1,v1) :: t1) as l1), (((k2,v2) :: t2) as l2) -> 
          let (accu', l1', l2') =
            let sign = Ord.compare k1 k2 in
            if sign > 0 then (k1, (Some v1, None)) :: accu, t1, l2
            else if sign < 0 then (k2, (None, Some v2)) :: accu, l1, t2
            else 
              (if eq v1 v2 then accu else ((k1, (Some v1, Some v2))::accu)), t1, t2 in
          diff_aux eq accu' (l1', l2') in
    diff_aux eq [] (lt1, lt2)
  let patch_gen eq pick diff t =
    let patch_fun t (k,d) = match pick d with
      | Some v1, v2  -> 
          if eq (find k t) v1 then
            let t' = remove k t in
            match v2 with Some v -> add k v t' | _ -> t'
          else raise Not_found
      | _, Some v2 -> add k v2 t
      | _, _ -> failwith "Not a valid patch" in
    List.fold_left patch_fun t diff
  let patch_left eq t diff = patch_gen eq (fun d -> d) diff t
  let patch_right eq diff t = patch_gen eq (fun (a,b) -> (b,a)) diff t
  let merge f t1 t2 = 
    let add_item k v tbl = 
      try f k v (find k tbl) tbl
      with Not_found -> add k v tbl in
    fold add_item t1 t2
end

open Obj

let obj_fields_from pos obj = 
  let rec walk acc = function
    | n when n >= pos -> walk (field obj n :: acc) (n - 1)
    | _ -> acc in
  walk [] (size obj - 1)

(* all prefix, depth first visit *)

let obj_find prop v =
  let obj = repr v in
  let already_seen = ref [] in
  let rec find_aux o = 
    if List.memq o !already_seen then None
    else if prop o then Some o 
    else
      (already_seen := o :: !already_seen;
       if tag o < no_scan_tag then 
         list_find_app find_aux (obj_fields_from 0 o)
       else None
      ) in
  find_aux obj

let obj_iter f v =
  let obj = repr v in
  let already_seen = ref [] in
  let rec iter_aux o =
    if not (List.memq o !already_seen) then
      (f o;
       already_seen := o :: !already_seen;
       if tag o < no_scan_tag then List.iter iter_aux (obj_fields_from 0 o)) in
  iter_aux obj

let obj_refed_by eq a b = 
  match obj_find (eq (repr a)) b with None -> false | Some _ -> true

(* Distructive substitution, note that rb is reference type because otherwise
   we have no way to substitute the whole expression, if it satify the
   condition *)
let obj_subst eq ((a: 'a), (a': 'a)) rb =
  let oa = repr a and ob = repr (!rb) and oa' = repr a' in
  let already_seen = ref [] in
  let rec subst_aux o =
    if not (List.memq o !already_seen) then 
      (already_seen := o :: !already_seen;
       if tag o < no_scan_tag then
         for i = 0 to size o do
           let oi = field o i in
           let equal = try eq oi oa with Invalid_argument _ -> oi == oa in
           if equal then set_field o i oa' else subst_aux oi
         done) in
  if eq ob oa  then rb := obj oa' else subst_aux ob

open Obj;;

let tag_delim = 246

let fields_from pos obj = 
  let rec walk acc = function
    | n when n >= pos -> walk (field obj n :: acc) (n - 1)
    | _ -> acc in
  walk [] (size obj - 1)

type pobj = 
    [`Int of int
    |`Tuple of pobj list    (* Record, List, Variants are just encoded tuples *)
    |`Lazy of closure
    |`Force of pobj         (* Note that some value being forced will have a
                               direct obj instead of a forward tag *)
    |`Closure of closure
    |`Object of clobj * int * (pobj list)
    |`String of string 
    |`Double of float
    |`Double_array of float array
    ]
and closure = codeptr * (pobj list)
and codeptr = Obj.t
and clobj = Obj.t  (* temporary representation *)

let rec parse_obj o : pobj = 
  match tag o with
  | x when x = int_tag -> `Int (obj o)
  | x when x = string_tag -> `String (obj o)
  | x when x = double_tag -> `Double (obj o)
  | x when x = double_array_tag -> `Double_array (obj o)
  | x when x = lazy_tag -> 
      assert (size o = 1);
      let obj = field o 0 in
      `Lazy (parse_clos obj)
  | x when x = forward_tag ->
      assert (size o = 1);
      `Force (parse_obj (field o 0))
  | x when x = closure_tag -> 
      `Closure (parse_clos o)
  | x when x = object_tag ->
      `Object (field o 0, obj (field o 1),
               List.map parse_obj (fields_from 2 o))
  | x when x = infix_tag -> invalid_arg "infix_tag"
  | x when x = no_scan_tag -> invalid_arg "no_scan_tag | abstract_tag"
  | x when x = custom_tag -> invalid_arg "custom_tag | final_tag"
  | _ -> `Tuple (List.map parse_obj (fields_from 0 o))
and parse_clos obj =
  assert (tag obj = closure_tag); 
  let code = field obj 0 in
  assert (tag code = out_of_heap_tag);
  (code, List.map parse_obj (fields_from 1 obj))

let parse x = parse_obj (repr x)


let rec list_find_app f = function
  | [] -> None
  | h::t -> match f h with Some _ as v -> v | None -> list_find_app f t

(* Prefix, depth first search *)
let find_obj prop obj =
  let already_seen = ref [] in
  let rec find_aux o = 
    if List.memq o !already_seen then None
    else if prop o then Some o 
    else
      (already_seen := o :: !already_seen;
       match tag o with
       | x when List.mem x
           [int_tag; string_tag; double_tag; double_array_tag] -> None
       | x when (x < tag_delim 
                 || List.mem x [closure_tag; object_tag; lazy_tag; forward_tag]) ->
              list_find_app find_aux (fields_from 0 o)
       | x -> prerr_int x; None
      ) in
  find_aux obj

let refed_by a b =  
  match find_obj ((==) (repr a)) (repr b) with
  | None -> false | Some _ -> true

(* Prefix, depth first travel. It's a destructive operation*)
let subst_obj sf obj =
  let already_seen = ref [] in
   let rec subst_aux o =
    if List.memq o !already_seen then o
    else match (sf o) with
    | Some x -> x 
    | None -> 
        (already_seen := o :: !already_seen;
         match tag o with
         | x when List.mem x 
             [int_tag; string_tag; double_tag; double_array_tag] -> o
         | x when (x < tag_delim || List.mem x
                     [closure_tag; object_tag; lazy_tag; forward_tag]) ->
             for i = 0 to size o - 1 do 
               let old_field = field o i in
               let new_field = subst_aux old_field in
               if old_field != new_field then set_field o i new_field
             done;
             o
         | x -> prerr_int x; o
        ) in
  subst_aux obj

let subst ((a: 'a), (a': 'a)) (b: 'b) : 'b =
  let obj_a = repr a in
  let obj_a' = repr a' in
  let sf o = if o == obj_a || o = obj_a then Some obj_a' else None in
  obj (subst_obj sf (repr b))


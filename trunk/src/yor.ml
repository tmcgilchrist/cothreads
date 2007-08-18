(* Now very dirty, specific to the simplest types we'll use in this project,
   to be cleaned up and generalised *)

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

(* We should really rewrite it with Format *)
let dump x = 
  let tree = parse x in
  let rec output = function
    | `Closure (codeptr, olist) -> 
        (Printf.sprintf "<fun:%d> (\n" (obj codeptr))
        ^ (String.concat "\n" (List.map output olist))
        ^ ")\n"
    | `Double f -> string_of_float f
    | `Force o -> "lazy (" ^ (output o) ^ ")"
    | `Double_array fa -> 
        let fl = Array.to_list fa in
        "[|" ^ (String.concat "; " (List.map string_of_float fl)) ^ "|]"
    | `Int i -> string_of_int i
    | `Lazy cl -> "<lazy>"
    | `Object (cl, i, ol) ->
        (Printf.sprintf "<obj:%d-%d> (\n" (obj cl) i)
        ^ (String.concat "\n" (List.map output ol))
        ^ ")\n"
    | `String s -> "\"" ^ s ^ "\""
    | `Tuple ol ->
        "(" ^ (String.concat ", " (List.map output ol)) ^ ")" in
  output tree

let print x = print_endline (dump x)

let rec list_find_app f = function
  | [] -> None
  | h::t -> match f h with Some _ as v -> v | None -> list_find_app f t

(* all prefix, depth first visit *)

let find prop v =
  let obj = repr v in
  let already_seen = ref [] in
  let rec find_aux o = 
    if List.memq o !already_seen then None
    else if prop o then Some o 
    else
      (already_seen := o :: !already_seen;
       if tag o < no_scan_tag then 
         list_find_app find_aux (fields_from 0 o)
       else None
      ) in
  find_aux obj

let iter f v =
  let obj = repr v in
  let already_seen = ref [] in
  let rec iter_aux o =
    if not (List.memq o !already_seen) then
      (f o;
       already_seen := o :: !already_seen;
       if tag o < no_scan_tag then List.iter iter_aux (fields_from 0 o)) in
  iter_aux obj

let refed_by eq a b = 
  match find (eq (repr a)) b with None -> false | Some _ -> true

(* The reason of return a 'a value rather than a unit is that, when the whole b is
   eq to a, we have no way to *physically* substitute it with 'a. By return a
   'a value, we ensure the a in every parts of b, ie include b itself, is
   substitued in the _final result_. Still, please remember, the operation is
   destructive in common cases, not functional. *)
let subst eq ((a: 'a), (a': 'a)) b =
  let oa = repr a and ob = repr b and oa' = repr a' in
  let subst_aux o = 
    if tag o < no_scan_tag then
      for i = 0 to size o do
        if eq oa (field o i) then set_field o i oa'
      done in
  if eq oa ob then (obj oa')
  else subst_aux ob; b

open Obj;;

let fields_from pos obj = 
  let rec walk acc = function
    | n when n >= pos -> walk (field obj n :: acc) (n - 1)
    | _ -> acc in
  walk [] (size obj - 1)

type pobj = 
    [`Int of int
    |`Tuple of pobj list    (* Record, List are just encoded tuples *)
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
  | x when x = lazy_tag -> 
      assert (size o = 1);
      let o' = field o 0 in
      assert (tag o' == closure_tag);
      `Lazy (field o' 0, List.map parse_obj (fields_from 1 o'))
  | x when x = forward_tag ->
      assert (size o = 1);
      `Force (parse_obj (field o 0))
  | x when x = closure_tag -> 
      `Closure (field o 0, List.map parse_obj (fields_from 1 o))
  | x when x = string_tag -> `String (obj o)
  | x when x = double_tag -> `Double (obj o)
  | x when x = double_array_tag -> `Double_array (obj o)
  | x when x = infix_tag -> invalid_arg "infix_tag"
  | x when x = no_scan_tag -> invalid_arg "no_scan_tag | abstract_tag"
  | x when x = custom_tag -> invalid_arg "custom_tag | final_tag"
  | _ -> `Tuple (List.map parse_obj (fields_from 0 o))


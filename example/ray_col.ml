module Thread = Cothread
open Ray

(* This kind of function should really be integrated into the Event module *)
let event_map ea' =
  let ea = Array.mapi (fun i e -> `Left (Event.wrap e (fun v -> (i, v)))) ea' in
  let rec run () = 
    let el = Array.fold_left (fun l -> function `Left e -> e::l | `Done _ -> l) [] ea in
    match el with 
    | [] -> Array.map (function `Done v -> v | `Left _ -> assert false) ea
    | _ -> let (i,v) = Event.select el in ea.(i) <- `Done v; run () in
  run ()

let (l,s,d,o) =
  try (int_of_string Sys.argv.(1), int_of_string Sys.argv.(2), 
       int_of_string Sys.argv.(3), Sys.argv.(4))
  with _ -> 
    let name = Filename.basename (Sys.argv.(0)) in
    Printf.fprintf stderr "Command: \"%s level size degree output\"\n" name;
    Printf.fprintf stderr "Launch: \"./%s 9 512 2 %s.pgm\"\n" name name;
    flush stderr; (9, 512, 2, name^".pgm")

let () = 
  let oc = open_out o in
  Printf.fprintf oc "P5\n%d %d\n255\n" s s;
  let pos = pos_out oc in
  let worker i = rasters l s d i in
  let output (off,ra) = 
    seek_out oc (pos+s*off); Array.iter (output_string oc) ra in
  let ea = Array.init d (fun i -> Event.wrap (Thread.spawn worker i) output) in
  ignore (event_map ea); close_out oc

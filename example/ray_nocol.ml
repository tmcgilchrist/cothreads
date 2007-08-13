module Thread=Cothread
open Ray

let (l,s,d,o) =
  try (int_of_string Sys.argv.(1), int_of_string Sys.argv.(2), 
       int_of_string Sys.argv.(3), Sys.argv.(4))
  with _ -> 
    let name = Filename.basename (Sys.argv.(0)) in
    Printf.fprintf stderr "Command: \"%s level size degree output\"\n" name;
    Printf.fprintf stderr "Launch defaults: \"./%s 9 512 2 %s.pgm\"\n" name name;
    (9, 512, 2, name^".pgm")

let () = 
  let oc = open_out o in
  Printf.fprintf oc "P5\n%d %d\n255\n" s s;
  let pos = pos_out oc in close_out oc;
  let output (off,ra) =
    let oc = open_out_gen [Open_wronly] 0o644 o in
    seek_out oc (pos+s*off); Array.iter (output_string oc) ra; close_out oc in
  let worker i = output (rasters l s d i) in
  let ta = Array.init d (Thread.create worker) in
  Array.iter (fun t -> Thread.join t) ta

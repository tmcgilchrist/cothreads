(* Simple example without much meanings, for test purpose *)
module Thread=Cothread
open Stm;;

let rec accu x = if x = 0 then 0 else x + (accu (pred x))

let simple () =
  let tv = tvar 0 in
  let trans x = read_tvar tv >>= fun v -> 
    Thread.delay (Random.float 0.001);
    if v >= accu (x/10) then return (atom (write_tvar tv (x + v)))
    else retry in
  let rec thread_fun x = 
    match atom_once (trans x) with 
    | None -> Printf.printf "%d fail\n" x; flush stdout; thread_fun x
    | Some _ -> Printf.printf "%d succ\n" x; flush stdout  in
  let thr_array = 
    Array.init 300 (fun x -> Thread.create thread_fun (x+1)) in
  Array.iter Thread.join thr_array;
  Printf.printf "Final result: %d\n" (atom (read_tvar tv)); flush_all ()

let _ = simple ()

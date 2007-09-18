(* Classical philosopher dinning problem written in STM. Launch it with [./phil
   n], where n is the number of philosophers and chopsticks.
*)
module Thread=Cothread (* Or use Thread directly *)
open Stm

(* Number of philosophers and chopsticks, the degree *)
let n = 
  try int_of_string Sys.argv.(1) 
  with Invalid_argument _ -> 
    prerr_endline "Launch the program with a single int, e.g. ./phil 5";
    exit 1

let chopstick = Array.init n (fun _ -> tvar true)

let left x = x and right x = (x + 1) mod n

let check b = if b then return () else retry

(* Actions: think, eat, takeup, putdown*)
let think x = 
  Printf.printf "Phil %d begins his THINKING ...\n" x; flush stdout;
  Thread.delay (Random.float 0.1)

let eat x = 
  Printf.printf "Phil %d begins to EAT -----> \n" x;
  Thread.delay (Random.float 0.02);
  Printf.printf "Phil %d now finish EAT <----- \n" x; flush stdout

let chop_act x s l r =
  Printf.printf "Phil %d %s chopstick %d and %d\n" x s l r

let takeup id =
  read_tvar chopstick.(id) >>= check >> write_tvar chopstick.(id) false

let putdown id =
  write_tvar chopstick.(id) true 

(* Philosopher thread function *)
let phil x =
  let l,r = (left x, right x) in
  let rec run () = 
    think x;
    atom (takeup l >> takeup r);
    chop_act x "take up" l r;
    eat x;
    atom (putdown l >> putdown r);
    chop_act x "put down" l r;
    run () in
  Random.self_init ();
  run ()

let main () =
  let phils = Array.init n (Thread.create phil) in
  Array.iter Thread.join phils

let _ = main ()

(* Classical philosopher dinning problem written in STM. Just a plain
   simulation, deadlock is possible because no preventing mechanics has been
   used. Launch it with [./phil n], where n is the number of philosophers and
   chopsticks.
*)

open Stm


(* Number of philosophers and chopsticks, the degree *)
let n = 
  try int_of_string Sys.argv.(1) 
  with Invalid_argument _ -> 
    prerr_endline "Launch the program with a single int, e.g. ./phil 5";
    exit 1

let chopstick = Array.init n (fun _ -> tvar true)

let left x = x and right x = (x + 1) mod n

(* Helper stm function *)
let check b = if b then return () else retry


(* Actions: think, eat, takeup, putdown*)

let think x = 
  Printf.printf "Phil %d begins his THINKING ...\n" x; flush stdout;
  Thread.delay (Random.float 0.1)

let eat x = 
  Printf.printf "Phil %d begins to EAT -----> \n" x;
  Thread.delay (Random.float 0.02);
  Printf.printf "Phil %d now finish EAT <----- \n" x; flush stdout

let takeup id side =
  let cs_id, cs_name = 
    match side with 
    | `Left -> left id, "_left_" 
    | `Right -> right id, "_right_" in
  atom (read_tvar chopstick.(cs_id) >>= check 
        >> write_tvar chopstick.(cs_id) false);
  Printf.printf "Phil %d take up chopstick %d on his %s\n" id cs_id cs_name;
  flush stdout

let putdown id =
    atom (write_tvar chopstick.(left id) true 
          >> write_tvar chopstick.(right id) true)


(* Philosopher thread function *)
let phil x =
  let rec run () = 
    think x;
    takeup x `Left;
    takeup x `Right;
    eat x;
    putdown x;
    run () in
  Random.self_init ();
  run ()


let main () =
  let phils = Array.init n (Thread.create phil) in
  Array.iter Thread.join phils

let _ = main ()

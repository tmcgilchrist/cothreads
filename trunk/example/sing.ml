module Thread=Cothread
open Stm

let tv = tvar 10

let rec run op t =
  Thread.delay (Random.float t);
  atom (read_tvar tv >>= fun x -> 
          Printf.printf "I read %d\n" x; flush_all ();
          write_tvar tv (op x));
  run op t

let th1 = Thread.create (run (fun x -> x * 2 + 1)) 0.5
let th2 = Thread.create (run (fun x -> x / 2)) 1.0
let _ = Thread.join th1; Thread.join th2

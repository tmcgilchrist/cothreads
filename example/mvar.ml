module Thread=Cothread
open Stm

type 'a mvar = 'a option tvar

let new_empty_mvar () =  tvar None


let take_mvar mv = 
  read_tvar mv >>= (function
    None -> retry
  | Some v ->
      write_tvar mv None >>= (function _ -> return v))

let put_mvar mv v =
  read_tvar mv >>= (function
    None -> write_tvar mv (Some v)
  | Some v -> retry)

let producer mv =
  let c = ref 0 in
  while true do
    Thread.delay (Random.float 0.2);
    atom (put_mvar mv !c);
    incr c
  done

let consumer mv =
  while true do
    Printf.printf "Receive %d\n" (atom (take_mvar mv));
    flush_all ();
  done

let main () =
  let mv = new_empty_mvar () in
  let prod = Thread.create producer mv in
  let consum = Thread.create consumer mv in
  Thread.join prod; Thread.join consum;
  ()

let () = main ()  

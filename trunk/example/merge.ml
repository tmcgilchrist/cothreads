module Thread=Cothread
open Stm

type 'a mvar = 'a option tvar

let new_empty_mvar =  new_tvar None

let take_mvar mv = 
  read_tvar mv >>= (function
    None -> retry
  | Some v ->
      write_tvar mv None >>= (function _ -> return v))

let put_mvar mv v =
  read_tvar mv >>= (function
    None -> write_tvar mv (Some v)
  | Some v -> retry)

let producer n mv =
  while true do
    Thread.delay (Random.float 0.2); 
    atom (put_mvar mv n);
  done

let merge mv1 mv2 =
  while true do
    Printf.printf "Receive signal from %d\n" 
      (atom (or_else (take_mvar mv1) (take_mvar mv2)));
      flush_all ()
  done

let main () =
  let mv1 = atom new_empty_mvar in
  let mv2 = atom new_empty_mvar in
  let prod1 = Thread.create (producer 1) mv1 in
  let prod2 = Thread.create (producer 2) mv2 in
  let consum = Thread.create (merge mv1) mv2 in
  Thread.join prod1; Thread.join prod2; Thread.join consum;
  ()

let () = main ()  


module Thread=Cothread
open Stm

type 'a chain = 'a item tvar and 
      'a item = Empty | Full of 'a * 'a chain

type 'a mchan = 'a chain tvar
type 'a port = 'a chain tvar

let new_mchan = new_tvar Empty >>= fun c -> new_tvar c
let new_port mc = read_tvar mc >>= fun c -> new_tvar c

let read_port p = 
  read_tvar p >>= fun c -> 
    read_tvar c >>= function
	Empty -> retry
      | Full (v, c') ->
	  write_tvar p c' >> return v

let write_mchan mc v =
  read_tvar mc >>= fun c ->
    new_tvar Empty >>= fun c' ->
      write_tvar c (Full (v, c')) >> write_tvar mc c'

let producer mc =
  let c = ref 0 in
  while true do
    Thread.delay (Random.float 0.2);
    atom (write_mchan mc !c);
    incr c
  done

let consumer n mc =
  let p = atom (new_port mc) in
  while true do
    Thread.delay (Random.float 0.2);
    Printf.printf "%d receives %d\n" n (atom (read_port p));
    flush_all ();
  done

let main () =
  let mc = atom (new_mchan) in
  let prod = Thread.create producer mc in
  let consum1 = Thread.create (consumer 1) mc in
  let consum2 = Thread.create (consumer 2) mc in
  Thread.join prod; Thread.join consum1; Thread.join consum2;
  ()

let () = main ()  

	


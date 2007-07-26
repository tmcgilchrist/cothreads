(* Example taken from the OReilly book *)

module Thread = Cothread
open Thread
open Libext

let create () =
  let perm = 0o600 in
  let my_id = id (self ()) in
  let name = filename_temp_filename "_pipe"
    (fun () -> Printf.sprintf "%08X" my_id) in
  let () = Unix.mkfifo name perm in
  let read_fd = Unix.openfile name [Unix.O_RDONLY; Unix.O_NONBLOCK] perm in
  let write_fd = Unix.openfile name [Unix.O_WRONLY] perm in
  let _ = Unix.unlink name in
  (read_fd, write_fd)
;;


let c = Condition.create () ;;
let m = Mutex.create ();;
let r,w = create ();;


let produce i p d = 
  incr p ;
  Thread.delay d ;
  Printf.printf "Producer (%d) has produced %d\n" i !p ;
  flush stdout ;;

let store2 i p =
  Mutex.lock m ;
  marshal_write (i,!p) w;
  Printf.printf "Producer (%d) has added its %dth product\n" i !p ;
  flush stdout ;
  Condition.signal c ;
  Mutex.unlock m ;;

let producer2 i =
  let p = ref 0 in
  let d = Random.float 2. 
  in while true do
    produce i p d;
    store2 i p;
    Thread.delay (Random.float 2.5)
  done ;; 

let wait2 i =
  Mutex.lock m ;
  while (let rr,_,_ = Unix.select [r] [] [] 0. in rr = []) do
    Printf.printf "Consumer (%d) is waiting\n" i ;
    Condition.wait c m
  done ;;

let take2 i =
  let ip, p = marshal_read r in
  Printf.printf "Consumer (%d) " i ;
  Printf.printf "takes product (%d, %d)\n" ip p ;
  flush stdout ;
  Mutex.unlock m ;;

let consumer2 i =
  while true do
    wait2 i;
    take2 i;
    Thread.delay (Random.float 2.5)
  done ;;


for i = 0 to 3 do
  ignore (Thread.create producer2 i);
  ignore (Thread.create consumer2 i)
done ;
while true do Thread.delay 5. done ;; 


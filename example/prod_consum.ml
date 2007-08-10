(* Example taken from the OReilly book *)

open Libext
open Coordinator

let create () =
  let name = fresh_name "_pipe" in
  let () = Unix.mkfifo name file_perm in
  let read_fd = Unix.openfile name [Unix.O_RDONLY; Unix.O_NONBLOCK] file_perm in
  let write_fd = Unix.openfile name [Unix.O_WRONLY] file_perm in
  let _ = Unix.unlink name in
  (read_fd, write_fd)
;;

let c = Condition.create () ;;
let m = Mutex.create ();;
let r,w = create ();;


let produce i p d = 
  incr p ;
  Cothread.delay d ;
  debug (fun _ -> Printf.printf "Producer (%d) has produced %d\n" i !p);
  Mutex.lock m ;
  debug (fun _ -> Printf.printf "Producer (%d) take the lock\n" i);
  marshal_write (i,!p) w;
  debug (fun _ -> Printf.printf "Producer (%d) has added its %dth product\n" i !p);
  Condition.signal c;
  debug (fun _ -> Printf.printf "Producer (%d) has signal others\n" i);
  Mutex.unlock m; 
  debug (fun _ -> Printf.printf "Producer (%d) has unlock it\n" i)

let producer2 i =
  let p = ref 0 in
  let d = Random.float 0.2 in
  try
  while true do
    produce i p d;
    Cothread.delay (Random.float 0.2);
  done 
  with Unix.Unix_error (e,_,_) -> 
    debug (fun _ -> Printf.printf "Producer (%d) exit because of %s" i (Unix.error_message e))

let wait2 i =
  Mutex.lock m ;
  debug (fun _ -> Printf.printf "Consumer (%d) take the lock\n" i);
  while (let rr,_,_ = Cothread.select [r] [] [] 0.0 in rr = []) do
    debug (fun _ -> Printf.printf "Consumer (%d) is waiting (and relase the lock)\n" i);
    Condition.wait c m;
    debug (fun _ -> Printf.printf "Consumer (%d) wakes up\n" i);
  done ;;

let take2 i =
  let ip, p = marshal_read r in
  debug (fun _ -> Printf.printf "Consumer (%d) takes product (%d, %d)\n" i ip p) ;
  Mutex.unlock m ;
  debug (fun _ -> Printf.printf "Consumer (%d) release the lock\n" i)  

let consumer2 i =
  try 
  while true do
    wait2 i;
    take2 i;
    Cothread.delay (Random.float 0.2);
  done
  with Unix.Unix_error (e,_,_) -> 
    debug (fun _ -> Printf.printf "Consumer (%d) exit because of %s" i (Unix.error_message e)) ;;

for i = 0 to 3 do
  ignore (Cothread.create producer2 i);
done ;
for i = 0 to 9 do
  ignore (Cothread.create consumer2 i)
done;

while true do Cothread.delay 5. done ;; 


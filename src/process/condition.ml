open Unix
open Libext
open Coordinator
open Cothread

type t = bool portal tunnel

let master_lock = Mutex.create ()

let create () = new_tunnel ()

let wait cond mut =
  let portal = create_portal () in
  write_tunnel portal cond;
  Mutex.unlock mut;
  if read_portal portal then (Mutex.lock mut; remove_portal portal)
  else assert false

let rec signal cond =
  Mutex.lock master_lock;
  let result = read_tunnel cond in
  Mutex.unlock master_lock;
  match result with Some portal -> write_portal true portal | None -> ()

let broadcast cond =
  let rec keep_read accu =
    match read_tunnel cond with 
    | Some portal -> keep_read (portal :: accu) 
    | None -> accu in
  (* Mutex.lock master_lock; *)
  let wait_lst = keep_read [] in
  (* Mutex.unlock master_lock; *)
  List.iter (fun portal ->  write_portal true portal) (List.rev wait_lst)

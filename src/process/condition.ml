open Unix
open Libext
open Coordinator
open Cothread


(* TODO: to reimplement with portal, tunnel is not persistent *)

type t = bool portal tunnel * Mutex.t

let create () = new_tunnel (), Mutex.create ()

let wait (t,_) mut =
  let portal = create_portal () in
  write_tunnel portal t;
  Mutex.unlock mut;
  if read_portal portal then (Mutex.lock mut; remove_portal portal)
  else assert false

let rec signal (t, m) =
  Mutex.lock m;
  let result = read_tunnel t in
  Mutex.unlock m;
  match result with Some portal -> write_portal true portal | None -> ()

let broadcast (t, m) =
  let rec keep_read accu =
    match read_tunnel t with 
    | Some portal -> keep_read (portal :: accu) 
    | None -> accu in
  Mutex.lock m;
  let wait_lst = keep_read [] in
  Mutex.unlock m;
  List.iter (fun portal ->  write_portal true portal) (List.rev wait_lst)

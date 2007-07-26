open Unix
open Libext
open Coordinator

type t = file_descr * file_descr

let create () =
  let perm = 0o600 in
  let my_id = id (self ()) in
  let name = filename_temp_filename "_condition"
    (fun () -> Printf.sprintf "%08X" my_id) in
  let () = mkfifo name perm in
  let read_fd = openfile name [O_RDONLY; O_NONBLOCK] perm in
  let write_fd = openfile name [O_WRONLY] perm in
  let _ = unlink name in
  (read_fd, write_fd)

let wait cond mut =
  let (_,w) = cond in
  let p = new_portal () in
  let _ = create_portal p in
  marshal_write p w;
  Mutex.unlock mut;
  if recv p then (remove_portal p; Mutex.lock mut)

let signal cond =
  let (r,_) = cond in
  try
    let p = marshal_read r in
    send true p
  with Unix_error ((EAGAIN|EWOULDBLOCK),_,_) -> ()

let broadcast cond =
  let (r, _) = cond in
  let rec try_signal () =
    let success = 
      try
        let p = marshal_read r in
        send true p; true
      with Unix_error ((EAGAIN|EWOULDBLOCK),_,_) -> false in
    if success then try_signal () in
  try_signal ()

open Unix
open Libext
open Coordinator

let rec force_create name =
  try openfile name [O_RDWR; O_CREAT; O_EXCL] 0o600
  with Unix_error (EEXIST,_,_) -> unlink name; force_create name

let lock_name, lock_fd = 
  let my_id = id (self ()) in
  let name = filename_temp_filename "_mutex" 
    (fun _ -> Printf.sprintf "%08X" my_id) in
  (* According to our naming convention, there shouldn't be any confliction on
     lockfile naming, since there can be only one running process for each
     pid. So we can use force_create, if lockfile already exists, it means it's
     lefted by previous execution *)
  let fd = force_create name in
  let _ = unlink name in
  name, fd

type t = int (* The offset *)

let create = 
  (* bits num of a unsigned int *)
  let usable_size = Sys.word_size - 2 in
  (* according to capability of threads num *)
  let bits_of_mutex = usable_size - bits_of_id in
  let r = ref 0 in
  fun () ->
    let id_part = id (self ()) in
    let mutex_part = r := (!r + 1) land (1 lsr bits_of_mutex - 1); !r in
    id_part lsl bits_of_mutex + mutex_part

let lock lk = 
  if lk <> lseek lock_fd lk SEEK_SET then assert false;
  lockf lock_fd F_LOCK 1

let try_lock lk =
  if lk <> lseek lock_fd lk SEEK_SET then assert false;
  try lockf lock_fd F_TLOCK 1; true
  with Unix_error (EACCES,_,_) | Unix_error (EAGAIN,_,_) -> false | e -> raise e

let unlock lk =
  if lk <> lseek lock_fd lk SEEK_SET then assert false;
  lockf lock_fd F_ULOCK 1







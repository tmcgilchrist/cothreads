open Unix
open Libext
open Coordinator

let lock_fd = 
  let lock_name = fresh_name "_mutex" in
  remove_exists lock_name; 
  let fd = openfile lock_name [O_WRONLY; O_CREAT] file_perm in
  remove_exists lock_name;
  fd

type t = int (* The offset *)

let create = fresh_number

let rec lock lk = 
  if lk <> lseek lock_fd lk SEEK_SET then assert false;
  lockf lock_fd F_LOCK 1

let try_lock lk =
  if lk <> lseek lock_fd lk SEEK_SET then assert false;
  try lockf lock_fd F_TLOCK 1; true
  with Unix_error (EACCES,_,_) | Unix_error (EAGAIN,_,_) -> false | e -> raise e

let unlock lk =
  if lk <> lseek lock_fd lk SEEK_SET then assert false;
  lockf lock_fd F_ULOCK 1







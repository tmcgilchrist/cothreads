open Unix
open Libext
open Coordinator
open Cothread

let lock_fd = 
  let lock_name = fresh_name "_mutex" in
  remove_exists lock_name; 
  let fd = openfile lock_name [O_WRONLY; O_CREAT] file_perm in
  remove_exists lock_name;
  fd

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







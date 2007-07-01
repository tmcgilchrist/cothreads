open Unix

type t = {pid : int}
type 'a portal = string


let self () = {pid = Unix.getpid ()}
let id t = t.pid
let exit () = exit 0
let kill t = Unix.kill (id t) Sys.sigkill


let perm = 0o600
let bsize = Marshal.header_size + 128


let portal () =
  let name = Filename.concat "ithread" (string_of_int (id (self ()))) in
  mkfifo name perm;
  name

let root = self ()
let root_portal = portal ()


(* We'll handle string length longer than BUF_SIZE later *)
let marshal_write v fd =
  let rec write_rec s ofs len =
    let len' = write fd s ofs len in
    if len' < len then write_rec s (ofs + len') (len - len') in
  let str = Marshal.to_string v [Marshal.Closures] in
  write_rec str 0 (String.length s)

let buf = String.create bsize
let marshal_read fd =
  let rec read_rec fd buf ofs len =
    let len' = read fd buf ofs len in
    if len' < len then read_rec fd buf (ofs + len') (len - len') in
  read_rec fd buf 0 Marshal.header_size;
  let data_size = Marshal.data_size buf 0 in
  let total_size = Marshal.header_size + data_size in
  let buf = 
    if total_size <= String.length buf then buf else
      let ext_buf = String.create total_size in
      String.blit buf 0 ext_buf 0 Marshal.header_size;
      ext_buf in
  read_rec fd buf Marshal.header_size data_size;
  Marshal.from_string buf 0

let send x p = 
  let fd = openfile p [O_WRONLY] perm in
  marshal_write x fd;
  close fd

let recv p =
  let fd = openfile p [O_RDONLY] perm in
  let v = marshal_read p in
  close fd;
  v











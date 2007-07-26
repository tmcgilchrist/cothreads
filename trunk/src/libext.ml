let debug_level = ref max_int

let debug ?(level = 0) f = if level <= !debug_level then f ()

let list_find_split =
  let rec find_rec test acc = function
    | [] -> raise Not_found
    | h :: t -> if test h then (acc, h, t) else find_rec test (h::acc) t in
  fun test l -> find_rec test [] l

(* The function doesn't gaurentee the nonexistance of destination file when
   it's going to be creating *)
let filename_temp_filename prefix suffix_fun =
  Filename.concat Filename.temp_dir_name (prefix^(suffix_fun ()))

let marshal_write v fd =
  let rec write_rec s ofs len =
    let len' = Unix.write fd s ofs len in
    if len' < len then write_rec s (ofs + len') (len - len') in
  let str = Marshal.to_string v [Marshal.Closures] in
  write_rec str 0 (String.length str)

let marshal_read fd =
  let bsize = Marshal.header_size + 128 in
  let buf = String.create bsize in
  let rec read_rec fd buf ofs len =
    let len' = Unix.read fd buf ofs len in
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

exception NoImplementationYet
let noimplementation x = raise NoImplementationYet

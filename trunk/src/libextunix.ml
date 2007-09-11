open Unix

(* Atomically write OCaml value to file_descr for both block/nonblock mode *) 
let marshal_write =
  let rec write_rec fd s ofs len =
    let len' = 
      try write fd s ofs len 
      with Unix_error ((EAGAIN|EWOULDBLOCK),_,_) when ofs > 0 -> 0 in
    match len' with
    | 0 -> ignore (select [] [fd] [] (-1.)); write_rec fd s ofs len
    | _ when len' < len -> write_rec fd s (ofs + len') (len - len')
    | _ -> () in
  fun v fd ->
    let str = Marshal.to_string v [Marshal.Closures] in
    write_rec fd str 0 (String.length str)

(* Atomically read OCaml value from file_descr for both block/nonblock mode *) 
let marshal_read fd =
  let bsize = Marshal.header_size + 128 in
  let buf = String.create bsize in
  let rec read_rec fd buf ofs len =
    let len' =
      try Some (read fd buf ofs len) with 
      | Unix_error (EAGAIN,_,_) 
      | Unix_error (EWOULDBLOCK,_,_) when ofs > 0 -> None
      | e -> raise e in
    match len' with
    | Some 0 -> raise End_of_file
    | Some l when l = len -> ()
    | Some l -> read_rec fd buf (ofs + l) (len -l)
    | None -> ignore (select [fd] [] [] (-1.)); read_rec fd buf ofs len in
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

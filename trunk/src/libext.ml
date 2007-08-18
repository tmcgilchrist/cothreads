open Unix

let debug_level = ref max_int

let debug ?(level = 0) f = 
  if level <= !debug_level then 
    (Printf.printf "(at %f: ) " (gettimeofday ());
     f (); 
     flush Pervasives.stdout)

exception Break
exception NoImplementationYet
let noimplementation x = raise NoImplementationYet

let list_find_split =
  let rec find_rec test acc = function
    | [] -> raise Not_found
    | h :: t -> if test h then (acc, h, t) else find_rec test (h::acc) t in
  fun test l -> find_rec test [] l

let bit_chop_to_n n x =
  let capability = 1 lsl n - 1 in
  x land capability

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

module Map_Make (Ord:Map.OrderedType) : sig
  include Map.S
  type 'a patch = (key * ('a option * 'a option)) list
  val diff: ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a patch
  val patch_left: ('a -> 'a -> bool) -> 'a t -> 'a patch -> 'a t
  val patch_right: ('a -> 'a -> bool) -> 'a patch -> 'a t -> 'a t
  val merge: (key -> 'a -> 'a -> 'a t -> 'a t) -> 'a t -> 'a t -> 'a t
end with type key = Ord.t = struct
  include Map.Make (Ord)
  type 'a patch = (key * ('a option * 'a option)) list
  let to_list t = fold (fun k v l -> (k,v)::l) t []  (* decrease order *)
  let diff eq t1 t2 =
    let lt1 = to_list t1 and lt2 = to_list t2 in
    let rec diff_aux eq accu = function
      | [], l -> List.fold_left (fun a (k, v) -> (k, (None, Some v))::a) accu l
      | l, [] -> List.fold_left (fun a (k, v) -> (k, (Some v, None))::a) accu l
      | (((k1,v1) :: t1) as l1), (((k2,v2) :: t2) as l2) -> 
          let (accu', l1', l2') =
            let sign = Ord.compare k1 k2 in
            if sign > 0 then (k1, (Some v1, None)) :: accu, t1, l2
            else if sign < 0 then (k2, (None, Some v2)) :: accu, l1, t2
            else 
              (if eq v1 v2 then accu else ((k1, (Some v1, Some v2))::accu)), t1, t2 in
          diff_aux eq accu' (l1', l2') in
    diff_aux eq [] (lt1, lt2)
  let patch_gen eq pick diff t =
    let patch_fun t (k,d) = match pick d with
      | Some v1, v2  -> 
          if eq (find k t) v1 then
            let t' = remove k t in
            match v2 with Some v -> add k v t' | _ -> t'
          else raise Not_found
      | _, Some v2 -> add k v2 t
      | _, _ -> failwith "Not a valid patch" in
    List.fold_left patch_fun t diff
  let patch_left eq t diff = patch_gen eq (fun d -> d) diff t
  let patch_right eq diff t = patch_gen eq (fun (a,b) -> (b,a)) diff t
  let merge f t1 t2 = 
    let add_item k v tbl = 
      try f k v (find k tbl) tbl
      with Not_found -> add k v tbl in
    fold add_item t1 t2
end

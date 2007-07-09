(* Extlib begins *)

let debug_level = ref max_int

let list_find_split =
  let rec find_rec test acc = function
    | [] -> raise Not_found
    | h :: t -> if test h then (acc, h, t) else find_rec test (h::acc) t in
  fun test l -> find_rec test [] l

let filename_temp_fname prefix suffix_fun =
  Filename.concat Filename.temp_dir_name (prefix ^ (suffix_fun ()))

(* Extlib ends *)


(* Framework begins *)

open Unix

type t = {pid: int} and ith = t
type 'a portal = string

let self () = {pid = Unix.getpid ()}
let id t = t.pid
let thread i = {pid = i}

let perm = 0o600
let bsize = Marshal.header_size + 128

let prng = Random.State.make_self_init ();;

let hex6 () =
  let rnd = (Random.State.bits prng) land 0xFFFFFF in
  Printf.sprintf "%06x" rnd

let new_portal () =
  filename_temp_fname ("ithread"^(string_of_int (id (self ())))) hex6

let create_portal p = 
  let rec try_create count =
    try mkfifo p perm
    with e -> 
      if count >= 100 then raise e 
      else 
        (String.blit (hex6 ()) 0 p ((String.length p) - 6) 6; 
         try_create (count + 1) ) in
  try_create 0

let remove_portal p =
  if Sys.file_exists p then Sys.remove p


(* We'll handle string length longer than BUF_SIZE later *)
let marshal_write v fd =
  let rec write_rec s ofs len =
    let len' = write fd s ofs len in
    if len' < len then write_rec s (ofs + len') (len - len') in
  let str = Marshal.to_string v [Marshal.Closures] in
  write_rec str 0 (String.length str)

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
  let v = marshal_read fd in
  close fd;
  v

let demand v w p =
  let tp = new_portal () in
  let pkg = w v tp in
  create_portal tp;
  send pkg p;
  let ack = recv tp in
  remove_portal tp;
  ack

let command c p = send c p

let services : (string * Obj.t list) list ref = ref []

let new_serv p f = 
  let f = Obj.repr f in
  let services' = 
    try
      let pre, (_, l), suc = list_find_split (fun (p', _) -> p == p') !services in
      List.rev_append pre ((p, (f :: l)) :: suc)
    with Not_found ->  (p, [f]) :: !services in
  services := services'

let del_serv p f = 
  let f = Obj.repr f in
  let services' = 
    let pre,(_, l),suc = list_find_split (fun (p', _) -> p == p') !services in
    let l_pre, _ ,l_suc = list_find_split ((==) f) l in
    match (List.rev_append l_pre l_suc) with
    | [] -> List.rev_append pre suc
    | nl -> List.rev_append pre ((p, nl) :: suc) in
  services := services'

let sub_serv p f1 f2 =
  let f1 = Obj.repr f1 in
  let f2 = Obj.repr f2 in
  let services' =
    let pre,(_,l),suc = list_find_split (fun (p', _) -> p == p') !services in
    let l_pre, _, l_suc = list_find_split ((==) f1) l in
    List.rev_append pre ((p, List.rev_append l_pre (f2 :: l_suc)) :: suc) in
  services := services'


module Ith_Set = Set.Make (struct type t = ith let compare = compare end)
let ith = ref Ith_Set.empty

let run_services f =
  let serv_conf = List.map
    (fun (p, fl) ->
       create_portal p;
       let fd = openfile p [O_RDWR] perm in
       (fd, List.rev_map Obj.obj fl)
    ) (List.rev !services) in
  let _ = f () in
  let fds , _ = List.split serv_conf in
  let rec run timeout =
    let ready, _, _ = select fds [] [] timeout in
    match ready with
    | [] -> ()
    | _ -> 
        List.iter 
          (fun fd ->
               let v = marshal_read fd in
               List.iter (fun f -> f v) (List.assoc fd serv_conf)
          ) ready;
        let new_timeout = if Ith_Set.is_empty !ith then 0. else (-1.) in
        run new_timeout in
  let error = try (run (-1.); None) with e -> Some e in
  List.iter close fds;
  List.iter (fun (p, _) -> remove_portal p) !services;
  match error with None -> () | Some e -> raise e

(* Framework ends *)



(* Root service begins *)

let root_portal = new_portal ()

let rec unreg () = 
  let flag = demand (self ()) (fun x p -> `Delete (x, p)) root_portal in
  if not flag then unreg ()

let rec reg ppid pid =
  let flag = 
    demand (ppid, pid) (fun (ppid,pid) p -> `Create (ppid, pid,p)) root_portal
  in if flag then () else reg ppid pid

let exit () = Pervasives.exit 0

let inited = ref false

let execute f x =
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->  exit ()));
  (try ignore (f x) with e -> prerr_endline (Printexc.to_string e));
  exit ()

let rec init () =
  assert (not !inited);
  inited := true; 
  let p = new_portal () in
  let _ = create_portal p in
  match fork () with
  | -1 -> inited := false; init ()
  | 0 -> 
      let b = recv p in
      let _ = remove_portal p in
      if b then (at_exit unreg; reg {pid = getppid ()} (self ())) else assert false
  | pid -> execute run_services (fun () -> send true p)

let rec create f x =
  if not !inited then init ();
  let p = new_portal () in
  let _ = create_portal p in
  match fork () with
  | -1 -> create f x
  | 0 -> if recv p then (remove_portal p; execute f x) else (remove_portal p; exit ())
  | pid ->
      let son = {pid = pid} in
      reg (self ()) son;
      send true p;
      son

let kill t = Unix.kill (id t) Sys.sigterm

let join t = 
  let success = demand t (fun t p -> `Wait (t, p)) root_portal in
  if not success then assert false

let test s =
  let ns = demand s (fun s p -> `Test (s, p)) root_portal in
  print_endline ns


module Ith_Map = Map.Make (struct type t = ith let compare = compare end)
type ith_info = { parent: t; wait_lst : bool portal list }
let ith_base = ref Ith_Map.empty

type root_msg = 
    [`Create of t * t * bool portal
    |`Delete of t * bool portal
    |`Wait of t * t * bool portal
    |`Test of string * string portal
    ]

let root_func = function
  | `Create (parent, son, p) -> 
      ith := Ith_Set.add son !ith;
      ith_base := Ith_Map.add son {parent = parent; wait_lst = []} !ith_base;
      send true p 
  | `Delete (self, p) -> 
      let {wait_lst = wl} = Ith_Map.find self !ith_base in
      List.iter (send true) wl; send true p;
      ith := Ith_Set.remove self !ith;
      ith_base := Ith_Map.remove self !ith_base
  | `Wait (t, p) ->
      (try
         let ith_info = Ith_Map.find t !ith_base in
         let new_info = {ith_info with wait_lst = p :: ith_info.wait_lst} in
         ith_base := Ith_Map.add t new_info !ith_base
       with Not_found -> send true p)
  | `Test (str, p) -> send ("Root got your msg "^str) p

let _ = new_serv root_portal root_func

(* Root service ends *)

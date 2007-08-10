open Libext
open Unix

type thread = int

(* We suppose the id limitation of threads is 16bit *)
let bits_of_id = 16
(* i.e. 0xFFFF *)
let capability = 1 lsl bits_of_id - 1 

let self () = Unix.getpid ()
let id t = t land capability
let thread id = assert (id <= capability); id 

let file_perm = 0o600
let dir_perm = 0o700

let work_dir_name = "cothread"
let work_dir = 
  let name = Filename.concat Filename.temp_dir_name work_dir_name in
  if not (Sys.file_exists name) then mkdir name dir_perm;
  name

(* fresh_name ensure that there won't exist name confliction between running
   processes as far as the process who created the name exists.
*)
let fresh_name =
  let tbl = Hashtbl.create 17 in
  fun prefix ->
    let self_id = id (self ()) in
    let counter = 
      try Hashtbl.find tbl (prefix, self_id)
      with Not_found -> let r = ref 0 in Hashtbl.add tbl (prefix, self_id) r; r in
    counter := !counter + 1 land max_int;
    let file_name = Printf.sprintf "%s%04X%08X" prefix self_id !counter in
    Filename.concat work_dir file_name

let remove_exists name = try unlink name with Unix_error (ENOENT,_,_) -> ()

module ThreadMap = Map.Make (struct type t = thread let compare = compare end)

type 'a portal = string

let create_portal () = 
  let portal = fresh_name "_portal" in
  remove_exists portal;
  mkfifo portal file_perm;
  portal

let remove_portal portal = remove_exists portal

let read_portal (p: 'a portal) : 'a  =
  let fd = openfile p [O_RDONLY] file_perm in
  let v = marshal_read fd in
  close fd;
  v

let poll_read_portal (p: 'a portal) : 'a =
  let fd = openfile p [O_RDONLY; O_NONBLOCK] file_perm in
  let data = 
    try Some (marshal_read fd)
    with End_of_file | Unix_error ((EAGAIN|EWOULDBLOCK),_,_) -> None in
  close fd;
  data

let write_portal (x: 'a)  (p: 'a portal) = 
  let fd = openfile p [O_WRONLY] file_perm in
  marshal_write x fd;
  close fd

let poll_write_portal (x : 'a) (p: 'a portal) =
  let fd = openfile p [O_RDWR; O_NONBLOCK] file_perm in
  try marshal_write x fd; Some (fun () -> close fd)
  with Unix_error ((EAGAIN|EWOULDBLOCK),_,_) -> None

let demand_portal (v: 'a) (w: 'a -> 'b portal -> 'c) (p: 'c portal) : 'b =
  let tp = create_portal () in
  let pkg = w v tp in
  write_portal pkg p;
  let ack = read_portal tp in
  remove_portal tp;
  ack

type 'a tunnel = file_descr * file_descr

let new_tunnel =
  let close_tunnel (r,w) = close r; close w in
  fun () -> 
    let tunnel_file = fresh_name "_tunnel" in
    remove_exists tunnel_file;
    mkfifo tunnel_file file_perm;
    let read_fd = openfile tunnel_file [O_RDONLY; O_NONBLOCK] file_perm in
    let write_fd = openfile tunnel_file [O_WRONLY] file_perm in
    remove_exists tunnel_file;
    let tunnel = read_fd, write_fd in
    Gc.finalise close_tunnel tunnel;
    tunnel

let read_tunnel (r, _) = 
  try Some (marshal_read r) with Unix_error ((EAGAIN|EWOULDBLOCK),_,_) -> None

let write_tunnel v (_, w) = marshal_write v w

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


let exn_handlers: (exn -> (unit -> 'a) -> 'a) list ref = ref []

let new_handler f = exn_handlers := f :: !exn_handlers

let rec handle_all e cont handlers = match handlers with
  | [] -> raise e
  | h :: t -> try h e cont with _ -> handle_all e cont t

let run_services () =
  let serv_conf = List.map
    (fun (p, fl) ->
       let fd = openfile p [O_RDWR] file_perm in
       (fd, List.rev_map Obj.obj fl)
    ) (List.rev !services) in
  let fds , _ = List.split serv_conf in
  let exn_handlers = List.rev !exn_handlers in
  let rec run () =
    let ready,_,_ = select fds [] [] (-1.) in
    match ready with
    | [] -> assert false
    | h :: _ -> 
        let excep = try 
          let v = marshal_read h in
          List.iter (fun f -> f v) (List.assoc h serv_conf);
          None
        with e -> Some e in
        match excep with
        | None -> run () 
        | Some e -> handle_all e run exn_handlers in
  let error = try run (); None with e -> Some e in
  List.iter close fds;
  List.iter (fun (p, _) -> remove_portal p) !services;
  match error with None -> () | Some e -> raise e



(* Root service begins *)

type root_msg = 
    [`Create of thread * thread * bool portal
    |`Delete of thread * bool portal
    |`Wait of thread * bool portal
    |`Test of string * string portal
    ]

let root_portal: root_msg portal = create_portal ()

type thread_info = {parent: thread; wait_lst: bool portal list}

let root_db = ref (ThreadMap.empty: thread_info ThreadMap.t)

exception Quit

let exit_handler e cont = match e with Quit -> () | e -> raise e

let root_func = function
  | `Create (parent, son, p) -> 
      root_db := ThreadMap.add son {parent = parent; wait_lst = []} !root_db;
      write_portal true p 
  | `Delete (self, p) -> 
      let {wait_lst = wl} = ThreadMap.find self !root_db in
      List.iter (write_portal true) wl; 
      write_portal true p;
      root_db := ThreadMap.remove self !root_db;
      if ThreadMap.is_empty !root_db then raise Quit
  | `Wait (t, p) ->
      (try
         let th_info = ThreadMap.find t !root_db in
         let new_info = {th_info with wait_lst = p :: th_info.wait_lst} in
         root_db := ThreadMap.add t new_info !root_db
       with Not_found -> write_portal true p)
  | `Test (str, p) -> write_portal ("Root got your msg "^str) p


let _ = new_serv root_portal root_func
let _ = new_handler exit_handler
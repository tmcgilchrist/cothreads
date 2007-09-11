open Unix
open Libext
open Libextunix


type thread = { pid: int }

let self () = {pid = Unix.getpid ()}
let parent () = {pid = Unix.getppid ()}
let id t = t.pid
let thread id = { pid = id }
let signal s t = Unix.kill s (id t)

let file_perm = 0o600
let dir_perm = 0o700

let work_dir_name = "cothread"
let work_dir = 
  let name = Filename.concat Filename.temp_dir_name work_dir_name in
  (try mkdir name dir_perm with Unix_error (EEXIST,_,_) -> ());
  name

(* fresh_number fresh_name ensure that there won't exist number/name
   confliction between running processes.
*)
let fresh_number =
  let usable_size = Sys.word_size -2 in
  let bits_of_id = 16 in (* Should be sufficient in most OS *)
  let bits_of_num = usable_size - bits_of_id in
  let counter = ref 0 in
  fun () ->
    let self_id = id (self ()) in
    let id_part = bit_chop_to_n bits_of_id self_id in
    let num_part = 
      counter := bit_chop_to_n bits_of_num (!counter + 1);
      !counter in
    (id_part lsl bits_of_num) + num_part

let fresh_name prefix =
  let num = fresh_number () in
  let file_name = Printf.sprintf "%s%0*X" prefix (Sys.word_size/4) num in
  Filename.concat work_dir file_name

let remove_exists name = try unlink name with Unix_error (ENOENT,_,_) -> ()

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

let poll_read_portal p =
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

let demand_portal f p =
  let tp = create_portal () in
  let pkg = f tp in
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


let exn_handlers: (exn -> (float -> 'a) -> 'a) list ref = ref []

let new_handler f = exn_handlers := f :: !exn_handlers

let rec handle_all e cont handlers = match handlers with
  | [] -> raise e
  | h :: t -> try h e cont with _ -> handle_all e cont t


module ThreadMap = Map_Make (struct type t = thread let compare = compare end)
module ThreadSet = Set.Make (struct type t = thread let compare = compare end)

type thread_info = {parent: thread; wait_lst: bool portal list}

let root_db = ref (ThreadMap.empty: thread_info ThreadMap.t)

let inited = ref false

let run_services () =
  let serv_conf = List.map
    (fun (p, fl) ->
       let fd = openfile p [O_RDWR] file_perm in
       (fd, List.rev_map Obj.obj fl)
    ) (List.rev !services) in
  let fds , _ = List.split serv_conf in
  let exn_handlers = List.rev !exn_handlers in
  let exns = ref [] in
  let rec run timeout =
    let ready,_,_ = select fds [] [] timeout in
    match ready with
    | [] -> ()
    | h :: _ -> 
        let excep = try 
          let v = marshal_read h in
          List.iter (fun f -> f v) (List.assoc h serv_conf);
          None
        with e -> Some e in
        match excep with
        | None -> 
            let timeout = if ThreadMap.is_empty !root_db then 0. else (-1.) in
            run timeout
        | Some e -> handle_all e run exn_handlers in
  (try run (-1.) with e -> 
     exns := e :: !exns;
     if ThreadMap.is_empty !root_db then ()
     else (ThreadMap.iter (fun thr _ -> signal Sys.sigterm thr) !root_db; run (-1.)));
  List.iter close fds;
  List.iter (fun (p, _) -> remove_portal p) !services;
  List.iter raise !exns


(* Root service begins *)

type root_msg = 
    [`Create of thread * thread * bool portal
    |`Delete of thread * bool portal
    |`Wait of thread * bool portal
    |`Test of string * string portal
    ]

let root_portal: root_msg portal = create_portal ()

exception Quit

let exit_handler e cont = match e with Quit -> () | e -> raise e

let root_func = function
  | `Create (t', t, p) -> 
      root_db := ThreadMap.add t {parent = t'; wait_lst = []} !root_db;
      write_portal true p 
  | `Delete (t, p) -> 
      let {wait_lst = wl} = ThreadMap.find t !root_db in
      List.iter (write_portal true) wl; 
      write_portal true p;
      root_db := ThreadMap.remove t !root_db;
      if ThreadMap.is_empty !root_db then raise Quit
  | `Wait (t, p) ->
      (try
         let th_info = ThreadMap.find t !root_db in
         let new_info = {th_info with wait_lst = p :: th_info.wait_lst} in
         root_db := ThreadMap.add t new_info !root_db
       with Not_found -> write_portal true p)
  | `Test (str, p) -> write_portal ("Root got your msg "^str) p

let rec unreg t = 
  let flag = demand_portal (fun p -> `Delete (t, p)) root_portal in
  if not flag then unreg t

let rec reg t' t =
  let flag = demand_portal (fun p -> `Create (t', t, p)) root_portal in 
  if not flag then reg t' t

let prefix_sig_handle s f =
  let old_handle = Sys.signal s Sys.Signal_default in
  let new_handle = Sys.Signal_handle 
    (fun _ -> f (); Sys.set_signal s old_handle; signal s (self ())) in
  Sys.set_signal s new_handle

let rec init () =
  assert (not !inited);
  (inited := true; 
   match fork () with
   | 0 -> 
       reg (parent ()) (self ());
       at_exit (fun () -> unreg (self ()));
       prefix_sig_handle Sys.sigterm (fun _ -> unreg (self ()));
       Sys.set_signal Sys.sigchld Sys.Signal_ignore
   | pid -> 
       run_services ();
       exit 0
  )


let _ = new_serv root_portal root_func
let _ = new_handler exit_handler

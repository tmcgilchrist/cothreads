open Libext
open Unix

type thread = int

let self () = Unix.getpid ()

let bits_of_id = 16
let capability = 1 lsl bits_of_id - 1

(* We suppose the number limitation of threads is 65535 *)
let id t = t land capability

let thread id = assert (id <= capability); id 

let exit () = Pervasives.exit 0

let kill t = Unix.kill (id t) Sys.sigterm

module ThreadMap = Map.Make (struct type t = thread let compare = compare end)


type 'a portal = string

let perm = 0o600

let new_portal =
  let fresh_name = 
    let r = ref 0 in
    fun () -> 
      let myid = (id (self ())) land 0xFFFF in
      let mynum = r := !r + 1 land 0x3FFFFFFF; !r in
      Printf.sprintf "%04X%08X" myid mynum in
  fun () -> filename_temp_filename "cothread" fresh_name

let create_portal p = 
  if not (Sys.file_exists p) then mkfifo p perm

let remove_portal p =
  if Sys.file_exists p then Sys.remove p


let send (x: 'a)  (p: 'a portal) = 
  let fd = openfile p [O_WRONLY] perm in
  marshal_write x fd;
  close fd

let recv (p: 'a portal) : 'a  =
  let fd = openfile p [O_RDONLY] perm in
  let v = marshal_read fd in
  close fd;
  v

let demand (v: 'a) (w: 'a -> 'b portal -> 'c) (p: 'c portal) : 'b =
  let tp = new_portal () in
  let pkg = w v tp in
  create_portal tp;
  send pkg p;
  let ack = recv tp in
  remove_portal tp;
  ack

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
       create_portal p;
       let fd = openfile p [O_RDWR] perm in
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

let root_portal: root_msg portal = new_portal ()

type thread_info = {parent: thread; wait_lst: bool portal list}

let root_db = ref (ThreadMap.empty: thread_info ThreadMap.t)

exception Quit

let exit_handler e cont = match e with Quit -> () | e -> raise e

let root_func = function
  | `Create (parent, son, p) -> 
      root_db := ThreadMap.add son {parent = parent; wait_lst = []} !root_db;
      send true p 
  | `Delete (self, p) -> 
      let {wait_lst = wl} = ThreadMap.find self !root_db in
      List.iter (send true) wl; 
      send true p;
      root_db := ThreadMap.remove self !root_db;
      if ThreadMap.is_empty !root_db then raise Quit
  | `Wait (t, p) ->
      (try
         let th_info = ThreadMap.find t !root_db in
         let new_info = {th_info with wait_lst = p :: th_info.wait_lst} in
         root_db := ThreadMap.add t new_info !root_db
       with Not_found -> send true p)
  | `Test (str, p) -> send ("Root got your msg "^str) p


let _ = new_serv root_portal root_func
let _ = new_handler exit_handler

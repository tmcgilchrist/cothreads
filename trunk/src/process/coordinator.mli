type thread
val bits_of_id : int
val self : unit -> thread
val id : thread -> int
val thread : int -> thread
val exit : unit -> unit
val kill : thread -> unit
module ThreadMap :
  sig
    type key = thread
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
val file_perm : int
val dir_perm: int
val fresh_name: string -> string
val remove_exists: string -> unit

type +'a portal
val create_portal : unit -> 'a portal
val remove_portal : 'a portal -> unit
val read_portal : 'a portal -> 'a
val write_portal : 'a -> 'a portal -> unit
val poll_read_portal: 'a portal -> 'a option
val poll_write_portal: 'a -> 'a portal -> (unit -> unit) option

val demand_portal : 'a -> ('a -> 'b portal -> 'c) -> 'c portal -> 'b

type +'a tunnel
val new_tunnel: unit -> 'a tunnel
val read_tunnel: 'a tunnel -> 'a option
val write_tunnel: 'a -> 'a tunnel -> unit

val new_serv : 'a portal -> ('a -> unit) -> unit
val del_serv : 'a portal -> ('a -> unit) -> unit
val sub_serv : 'a portal -> ('a -> unit) -> ('a -> unit) -> unit
val exn_handlers : (exn -> (unit -> unit) -> unit) list ref
val new_handler : (exn -> (unit -> unit) -> unit) -> unit
val handle_all : exn -> 'a -> (exn -> 'a -> 'b) list -> 'b
val run_services : unit -> unit
type root_msg =
    [ `Create of thread * thread * bool portal
    | `Delete of thread * bool portal
    | `Test of string * string portal
    | `Wait of thread * bool portal ]
val root_portal : root_msg portal
type thread_info = { parent : thread; wait_lst : bool portal list; }
val root_db : thread_info ThreadMap.t ref
exception Quit
val exit_handler : exn -> 'a -> unit
val root_func : root_msg -> unit

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
type +'a portal
val perm : int
val new_portal : unit -> 'a portal
val create_portal : 'a portal -> unit
val remove_portal : 'a portal -> unit
val send : 'a -> 'a portal -> unit
val recv : 'a portal -> 'a
val demand : 'a -> ('a -> 'b portal -> 'c) -> 'c portal -> 'b
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

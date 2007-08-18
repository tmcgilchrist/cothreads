type thread
val self : unit -> thread
val parent : unit -> thread
val id : thread -> int
val thread : int -> thread
val signal : int -> thread -> unit
val file_perm : int
val dir_perm : int
val work_dir_name : string
val work_dir : string
val fresh_number : unit -> int
val fresh_name : string -> string
val remove_exists : string -> unit
type 'a portal
val create_portal : unit -> 'a portal
val remove_portal : 'a portal -> unit
val read_portal : 'a portal -> 'a
val poll_read_portal : 'a portal -> 'a option
val write_portal : 'a -> 'a portal -> unit
val poll_write_portal : 'a -> 'a portal -> (unit -> unit) option
val demand_portal : ('a portal -> 'b) -> 'b portal -> 'a
type 'a tunnel
val new_tunnel : unit -> 'a tunnel
val read_tunnel : 'a tunnel -> 'a option
val write_tunnel : 'a -> 'a tunnel -> unit
val new_serv : 'a portal -> ('a -> unit) -> unit
val del_serv : 'a portal -> ('a -> unit) -> unit
val sub_serv : 'a portal -> ('a -> unit) -> ('a -> unit) -> unit
val new_handler : (exn -> (float -> unit) -> unit) -> unit
val run_services : unit -> unit
type root_msg =
    [ `Create of thread * thread * bool portal
    | `Delete of thread * bool portal
    | `Test of string * string portal
    | `Wait of thread * bool portal ]
val root_portal : root_msg portal
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
    type 'a patch = (key * ('a option * 'a option)) list
    val diff : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a patch
    val patch_left : ('a -> 'a -> bool) -> 'a t -> 'a patch -> 'a t
    val patch_right : ('a -> 'a -> bool) -> 'a patch -> 'a t -> 'a t
    val merge : (key -> 'a -> 'a -> 'a t -> 'a t) -> 'a t -> 'a t -> 'a t
  end
module ThreadSet :
  sig
    type elt = thread
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val exit_handler : exn -> 'a -> unit
val root_func : root_msg -> unit
val inited : bool ref
val init : unit -> unit
val reg: thread -> thread -> unit
val unreg: thread -> unit

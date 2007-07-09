type t

val self : unit -> t
val id : t -> int
val thread: int -> t

val create: ('a -> 'b) -> 'a -> t
val exit: unit -> unit
val join: t -> unit
val kill: t -> unit

val test: string -> unit

(**/**)

type 'a portal

val new_portal: unit -> 'a portal
val create_portal: 'a portal -> unit
val remove_portal: 'a portal -> unit


(* Only root node can manipulate service *)
val new_serv: 'a portal -> ('a -> unit) -> unit
val del_serv: 'a portal -> ('a -> unit) -> unit
val sub_serv: 'a portal -> ('a -> unit) -> ('a -> unit) -> unit

val send: 'a -> 'a portal -> unit
val recv: 'a portal -> 'a

val demand: 'a -> ('a -> 'b portal -> 'c) -> 'c portal -> 'b
val command: 'a -> 'a portal -> unit



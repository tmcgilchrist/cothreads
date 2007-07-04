type t

val self : unit -> t
val id : t -> int

val exit: unit -> unit
val kill: t -> unit


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

(*
val demand: ([> ] as 'a) -> 'b portal -> 'b 
val command: ([> ] as 'a) -> unit
*)

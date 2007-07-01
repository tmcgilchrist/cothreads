type t

val self : unit -> t
val id : t -> int

val create: ('a -> 'b) -> 'a -> t
val exit: unit -> unit
val kill: t -> unit
val join: t -> unit


(**/**)

type 'a portal

val portal: unit -> 'a portal
val send: 'a -> 'a portal -> unit
val recv: 'a portal -> 'a

(* Only root node can register service *)
val service: (([> ] as 'a)  -> unit) -> unit 
val demand: 'a -> 'b portal -> 'b
val command: 'a -> unit











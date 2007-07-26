type t
val self : unit -> t
val id : t -> int
val exit : unit -> unit
val kill : t -> unit
val delay : float -> unit

val create : ('a -> 'b) -> 'a -> t
val join : t -> unit
val test : string -> unit

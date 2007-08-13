type t
val create : ('a -> 'b) -> 'a -> t
val self : unit -> t
val id : t -> int 
val exit : unit -> unit
val kill : t -> unit
val delay: float -> unit
val join : t -> unit
val wait_read : Unix.file_descr -> unit
val wait_write : Unix.file_descr -> unit
val wait_timed_read : Unix.file_descr -> float -> bool
val wait_timed_write : Unix.file_descr -> float -> bool
val select :
  Unix.file_descr list -> Unix.file_descr list ->
  Unix.file_descr list -> float ->
  Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
val wait_pid : int -> int * Unix.process_status
val yield : unit -> unit
val wait_signal : int list -> int

(* Extended functions *)
val spawn: ('a -> 'b) -> 'a -> 'b Event.event
val spawnl: ('a -> 'b) -> 'a -> 'b Event.event

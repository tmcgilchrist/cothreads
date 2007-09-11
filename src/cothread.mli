(** Super set of standard Thread module. 

    It serves two purpose: 1) a unified threads maniputation interface among
    different engines, so that we can achieve object-level compatability 2)
    extended functions of threads maniputation *)

(** {6 The part compatible with standard Thread module} *)

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


(** {6 Extended functions} *)

(** The interface of [spawn] and [spawnl] may change in the future. We have
    find a more elegent and comprehensive way to abstract the whole family of
    this kind of computation. Just need more time to implement.
*)


(** [spawn f x] launch up the computation of [(f x)] in a separate thread right
    away, the result is return as a event which you can [sync] with. Re-sync
    with the same event will bring you the same result, the computation won't
    repeat.
*)
val spawn: ('a -> 'b) -> 'a -> 'b Event.event

(** [spawnl f x] returns a event represents the computation of [(f x)] as a
    separate thread, just like [spwan]. However the computation will be defered
    until it's been [sync] with. Whenever you [sync] the event, a new
    thread is relaunched to do the same computation
*)
val spawnl: ('a -> 'b) -> 'a -> 'b Event.event


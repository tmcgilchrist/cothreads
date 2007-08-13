open Unix
open Libext
open Coordinator

type t = thread

let self = self
let id = id
let exit () = Pervasives.exit 0
let kill = signal Sys.sigterm 

let rec unreg t = 
  let flag = demand_portal t (fun x p -> `Delete (x, p)) root_portal in
  if not flag then unreg t

let rec reg t' t =
  let flag = demand_portal t (fun t p -> `Create (t', t, p)) root_portal in 
  if not flag then reg t' t

let prefix_sig_handle s f =
  let temp_signal_handle = ref Sys.Signal_default in
  temp_signal_handle :=
    Sys.signal s
      (Sys.Signal_handle 
         (fun _ -> 
            f ();
            Sys.set_signal s !temp_signal_handle;
            signal s (self ())))

let inited = ref false

let rec init () =
  assert (not !inited);
  inited := true; 
  match fork () with
  | 0 -> 
      reg (parent ()) (self ());
      prefix_sig_handle Sys.sigterm (fun _ -> unreg (self ()));
      at_exit (fun () -> unreg (self ()))
  | pid -> 
      run_services ();
      exit ()

let create f x =
  flush_all ();
  if not !inited then init ();
  match fork () with
  | 0 -> 
      let self = self () in
      let parent = parent () in
      reg parent self;
      let error = try ignore (f x); None with e -> Some e in
      (match error with None -> exit () | Some e -> unreg self; raise e)
  | pid -> 
      thread pid

let join t = 
  let success = demand_portal t (fun t p -> `Wait (t, p)) root_portal in
  if not success then assert false

let select = Unix.select

let delay d = ignore (select [] [] [] d)

let wait_read fd = ignore (select [fd] [] [] (-1.))
let wait_write fd = ignore (select [fd] [] [] (-1.))
let wait_timed_read fd time = 
  match select [fd] [] [] time with [],_,_ -> false | _ -> true
let wait_timed_write fd time = 
  match select [fd] [] [] time with [],_,_ -> false | _ -> true

let wait_pid pid = Unix.waitpid [] pid  

let yield () = ()

let sigmask = Unix.sigprocmask

let wait_signal sigs =
  let gotsig = ref 0 in
  let sighandler s = gotsig := s in
  let oldhdlrs =
    List.map (fun s -> Sys.signal s (Sys.Signal_handle sighandler)) sigs in
  if !gotsig = 0 then Unix.sigsuspend sigs;
  List.iter2 Sys.set_signal sigs oldhdlrs;
  !gotsig

let spawn f x = 
  let ch = Event.new_channel () in
  let result = ref `Unknown in
  let thread_fun () =
    let res = try `Result (f x) with e -> `Exn e in
    Event.sync (Event.send ch res) in
  ignore (create thread_fun ());
  let rec launch () = match !result with 
    | `Result v -> Event.always v
    | `Exn e -> raise e
    | `Unknown -> 
        Event.wrap (Event.receive ch) 
          (fun res -> result:= res; Event.sync (launch ())) in
  Event.guard launch

let spawnl f x =
  let ch = Event.new_channel () in
  let thread_fun () = Event.sync (Event.send ch (f x)) in
  let launch () = 
    let worker = create thread_fun () in
    Event.wrap_abort (Event.receive ch) (fun () -> kill worker) in
  Event.guard launch


(*
let test s =
  let ns = demand_portal s (fun s p -> `Test (s, p)) root_portal in
  print_endline ns
*)

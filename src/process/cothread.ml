open Unix
open Libext
open Coordinator

type t = thread

let self = self
let id = id
let exit () = Pervasives.exit 0

let execute f x =
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->  exit ()));
  let exit_code = 
    try ignore (f x); 0 
    with e -> (prerr_endline (Printexc.to_string e); 1) in
  Pervasives.exit exit_code

let rec unreg () = 
  let flag = demand_portal (self ()) (fun x p -> `Delete (x, p)) root_portal in
  if not flag then unreg ()

let rec reg ppid pid =
  let flag = demand_portal (ppid, pid) 
    (fun (ppid,pid) p -> `Create (ppid, pid, p)) root_portal in 
  if flag then () else reg ppid pid

let inited = ref false

let rec init () =
  assert (not !inited);
  inited := true; 
  let root = self () in
  match fork () with
  | -1 -> inited := false; init ()
  | 0 -> at_exit unreg; reg root (self ())
  | pid -> execute run_services ()

let rec create f x =
  flush_all ();
  Gc.full_major ();
  if not !inited then init ();
  let p = create_portal () in
  match fork () with
  | -1 -> create f x
  | 0 -> if read_portal p then (remove_portal p; execute f x) else (remove_portal p; assert false)
  | pid ->
      let son = thread pid in
      reg (self ()) son;
      write_portal true p;
      son

let kill t = Unix.kill (id t) Sys.sigterm

let delay d = ignore (select [] [] [] d)

let join t = 
  let success = demand_portal t (fun t p -> `Wait (t, p)) root_portal in
  if not success then assert false

let select = Unix.select

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

(*
let test s =
  let ns = demand_portal s (fun s p -> `Test (s, p)) root_portal in
  print_endline ns
*)

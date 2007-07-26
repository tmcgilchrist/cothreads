open Unix
open Libext
open Coordinator

type t = thread

let self = self
let id = id
let exit = exit
let kill = kill
let delay d = ignore (select [] [] [] d)


let inited = ref false

let execute f x =
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->  exit ()));
  let exit_code = 
    try ignore (f x); 0 
    with e -> (prerr_endline (Printexc.to_string e); 1) in
  Pervasives.exit exit_code

let rec unreg () = 
  let flag = demand (self ()) (fun x p -> `Delete (x, p)) root_portal in
  if not flag then unreg ()

let rec reg ppid pid =
  let flag = demand (ppid, pid) 
    (fun (ppid,pid) p -> `Create (ppid, pid, p)) root_portal in 
  if flag then () else reg ppid pid

let rec init () =
  assert (not !inited);
  inited := true; 
  let _ = create_portal root_portal in
  match fork () with
  | -1 -> inited := false; remove_portal root_portal; init ()
  | 0 -> at_exit unreg; reg (thread (getppid ())) (self ())
  | pid -> execute run_services ()

let rec create f x =
  if not !inited then init ();
  let p = new_portal () in
  let _ = create_portal p in
  match fork () with
  | -1 -> create f x
  | 0 -> if recv p then (remove_portal p; execute f x) else (remove_portal p; Pervasives.exit 2)
  | pid ->
      let son = thread pid in
      reg (self ()) son;
      send true p;
      son

let join t = 
  let success = demand t (fun t p -> `Wait (t, p)) root_portal in
  if not success then assert false

let test s =
  let ns = demand s (fun s p -> `Test (s, p)) root_portal in
  print_endline ns


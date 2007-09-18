(* Or just use Thread *)
module Thread=Cothread

(* Test 1 *)
let ch1 = Event.new_channel ()

let r = Event.choose (Array.to_list (Array.init 10 Event.always));;

let ntimes n = 
  for i = 0 to (n-1) do 
    match Event.poll r with Some i -> print_int i | _ -> assert false
  done;;

(* Test 2 *)
let e1 = Event.guard (fun () -> print_endline "Guard"; Event.always 999)
let e2 = Event.wrap_abort (Event.receive ch1) (fun _ -> print_endline "Fail")
let e = Event.wrap (Event.choose [e1; e2]) (Printf.printf "Result: %d\n")
let kinds () = Event.sync e; print_endline "End!"


(* Test 3 *)
let ch2 = Event.new_channel ()
let f1 () = Event.sync (Event.send ch2 8888)
let f2 () = Event.sync (Event.wrap (Event.receive ch2) print_int)
let f3 () =
  ignore (Thread.create f1 ());
  ignore (Thread.create f2 ())

(* Test 4 *)

let c = Event.new_channel ();;
let f () =
  let ids = string_of_int (Thread.id (Thread.self ())) 
  in print_string ("-------- before  -------" ^ ids) ; print_newline() ;
  let e = Event.receive c 
  in print_string ("-------- during  -------" ^ ids) ; print_newline() ;
  let v = Event.sync e 
  in print_string (v ^ " " ^ ids ^ " ") ; 
  print_string ("-------- after  -------" ^ ids) ; print_newline() ;;

let g () =
  let ids = string_of_int (Thread.id (Thread.self ())) 
  in print_string ("Start of " ^ ids ^ "\n");
  let e2 = Event.send c "hello" 
  in Event.sync e2 ;
  print_string ("End of " ^ ids) ;
  print_newline () ;;

let _ = 
  ntimes 100;
  kinds ();
  f3 ();
  let t1 = Thread.create f () in
  let t2 = Thread.create f () in
  let t3 = Thread.create g () in
  Thread.join t1;
  Thread.join t2;
  Thread.join t3

open Ithread

let test_f () = 
  let r = Random.int 100 in
  for i = 0 to r do
    test (Printf.sprintf "%dth test from %d\n" i (id (self ())))
  done

let ith_array = Array.init 7 (fun _ -> create test_f ()) 
let _ = Array.iter (fun t -> join t) ith_array

open Cothread

let lk = Mutex.create ()

let test_f () = 
  let myid = id (self ()) in
  let r = Random.int 10000 in
  for i = 0 to r do
    test (Printf.sprintf "%dth test from %d\n" i myid);
    while (not (Mutex.try_lock lk)) do 
      Printf.printf "%d fail to get the mutex\n" myid;
    done;
    Printf.printf "%d finally get the mutex\n" myid;
    Mutex.unlock lk
  done

let ith_array = Array.init 7 (fun _ -> create test_f ()) 
let _ = Array.iter (fun t -> join t) ith_array

open Thread

let lk1 = Mutex.create ()
let lk2 = Mutex.create ()

let rec run x =
  Mutex.lock lk1;
  Printf.printf "%d takes lock 1\n" x; flush stdout;
  Mutex.lock lk2;
  Printf.printf "%d takes lock 2\n" x; flush stdout;
  Mutex.unlock lk2;
  Printf.printf "%d release lock 2\n" x; flush stdout;
  Mutex.unlock lk1;
  Printf.printf "%d release lock 1\n" x; flush stdout;
  Thread.delay (Random.float 0.2);
  run x

let _ = 
  ignore (Array.init 10 (Thread.create run));
  while true do Thread.delay 5.0 done

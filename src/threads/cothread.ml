include Thread

let spawn f x = 
  let ch = Event.new_channel () in
  let result = ref `Unknown in
  let thread_fun () =
    let res = try `Result (f x) with e -> `Exn e in
    Event.sync (Event.send ch res) in
  ignore (Thread.create thread_fun ());
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
    let worker = Thread.create thread_fun () in
    Event.wrap_abort (Event.receive ch) (fun () -> Thread.kill worker) in
  Event.guard launch

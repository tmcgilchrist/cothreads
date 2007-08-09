open Unix
open Coordinator

type id = int

type 'a behavior = 
    {poll: unit -> bool; 
     suspend: unit -> (unit -> unit) option;
     result: unit -> 'a;
     handler: Obj.t portal -> unit;
     abort: (unit -> unit) list;
    }

type 'a channel = 
    {chn_lock: Mutex.t;
     send_pendings: communicator portal;
     recv_pendings: communicator portal; }

and communicator =
    { com_lock: Mutex.t;
      com_portal: (id * Obj.t portal) portal;
      evn_number: id; }

type 'a event = 
  | Communication of (communicator -> 'a behavior)
  | Choose of 'a event list
  | Guard of (unit -> 'a event)

let new_channel () =
  let lk = Mutex.create () in
  let sp = create_portal () in
  let rp = create_portal () in
  {chn_lock = lk; send_pendings = sp; recv_pendings = rp}


let protect_do mut f x =
  Mutex.lock mut;
  let res = f x in
  Mutex.unlock mut;
  res

let rec poll_recv_pending portal data =
  match poll_read_portal portal with
  | None -> None
  | Some comm  ->
      let action () =
        let answer_portal = create_portal () in
        let res = try 
          write_portal (comm.evn_number, answer_portal) comm.com_portal;
          write_portal (Obj.repr data) answer_portal;
          Some ()
        with Unix_error (ENOENT,_,_) -> None in
        remove_portal answer_portal;
        res in
      match protect_do comm.com_lock action () with
      | None -> poll_recv_pending portal data
      | x -> x

let rec poll_send_pending portal =
  match poll_read_portal portal with
  | None -> None
  | Some comm ->
      let action () =
        let answer_portal = create_portal () in
        let res =
          try
            write_portal (comm.evn_number, answer_portal) comm.com_portal;
            Some (Obj.obj (read_portal answer_portal))
          with Unix_error (ENOENT,_,_) -> None in
        remove_portal answer_portal;
        res in
      match protect_do comm.com_lock action () with
      | None -> poll_send_pending portal
      | x -> x

let send channel data : _ event =  Communication 
  (fun comm ->
     let result = ref None in
     { poll = 
         (fun () ->
            result := 
              protect_do channel.chn_lock (poll_recv_pending channel.recv_pendings) data;
            match !result with None -> false | _ -> true
         );
       suspend =
         (fun () ->
            protect_do channel.chn_lock (poll_write_portal comm) channel.send_pendings 
         );
       handler = (fun portal -> write_portal (Obj.repr data) portal; result := Some ());
       result = 
         (fun () -> match !result with None -> assert false | Some () -> ());
       abort = [];
     }
  )

let receive channel : _ event = Communication 
  (fun comm ->
     let result = ref None in
     {poll = 
         (fun () ->
            result := 
              protect_do channel.chn_lock poll_send_pending channel.send_pendings;
            match !result with None -> false | _ -> true);
      suspend =
         (fun () -> 
            protect_do channel.chn_lock (poll_write_portal comm) channel.recv_pendings
         );
      handler = (fun portal -> result := Some (Obj.obj (read_portal portal)));
      result =
         (fun () -> match !result with None -> assert false | Some x -> x);
      abort = [];
     }
  )

let always data : _ event = Communication
  (fun comm ->
     { poll = (fun () -> true);
       suspend = (fun () -> Some (fun () -> ()));
       result = (fun () -> data);
       handler = (fun _ -> assert false);
       abort = [];
     }
  )

let choose evl = Choose evl

let rec wrap_abort ev fn = match ev with
  | Communication genev ->
      Communication (fun comm -> 
                       let bev = genev comm in
                       { bev with abort = fn :: bev.abort }
                    )
  | Choose evl -> Choose (List.map (fun ev -> wrap_abort ev fn) evl)
  | Guard gu -> Guard (fun () -> wrap_abort (gu ()) fn)


let guard fn = Guard fn

let rec wrap ev fn = match ev with
  | Communication genev ->
      Communication (fun comm ->
                       let bev = genev comm in
                       { bev with result = fun () -> fn (bev.result ()) }
                    )
  | Choose evl -> Choose (List.map (fun ev -> wrap ev fn) evl)
  | Guard gu -> Guard (fun () -> wrap (gu ()) fn)


let rec flatten_event = function
  | Communication ev -> [ev]
  | Choose evl -> List.flatten (List.map flatten_event evl)
  | Guard fn -> flatten_event (fn ())


let scramble_array a =
  let len = Array.length a in
  if len = 0 then invalid_arg "Event.choose";
  for i = len - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = a.(i) in a.(i) <- a.(j); a.(j) <- temp
  done;
  a

let do_aborts bev = function
  | None -> Array.iter (fun ev -> List.iter (fun f ->  f ()) ev.abort) bev
  | Some n ->
      Array.iteri 
        (fun i ev -> 
           if i <> n then
             List.iter 
               (fun f -> if not (List.exists ((==) f) bev.(n).abort) then f ())
               ev.abort
        ) bev

let poll ev =
  let dummy_lock = Mutex.create () in
  let dummy_portal = create_portal () in
  let _ = remove_portal dummy_portal in
  let eva = scramble_array (Array.of_list (flatten_event ev)) in
  let bev = Array.init (Array.length eva) 
    (fun i -> eva.(i) { com_lock = dummy_lock; 
                        com_portal = dummy_portal;
                        evn_number = i }
    ) in
  let rec poll_events i =
    if i >= Array.length bev then None
    else if (bev.(i).poll) () then Some i
    else poll_events (i+1) in
  let evn_num = poll_events 0 in
  do_aborts bev evn_num;
  match evn_num with Some n -> Some (bev.(n).result ()) | None -> None

let sync ev =
  let com_lock = Mutex.create () in
  let com_portal = create_portal () in
  let clean_up = ref [] in
  let eva = scramble_array (Array.of_list (flatten_event ev)) in
  let bev = Array.init (Array.length eva)
    (fun i -> eva.(i) { com_lock = com_lock;
                        com_portal = com_portal;
                        evn_number = i }
    ) in
  let get_final i =
    List.iter (fun f -> f ()) !clean_up;
    do_aborts bev (Some i);
    bev.(i).result () in
  let rec sync_events i =
    if i >= Array.length bev || not (Mutex.try_lock com_lock) 
    then 
      let i, portal = read_portal com_portal in
      remove_portal com_portal;
      bev.(i).handler portal;
      get_final i
    else 
      if bev.(i).poll () then 
        (remove_portal com_portal;
         Mutex.unlock com_lock;
         get_final i) 
      else
        match bev.(i).suspend () with
        | Some f -> 
            clean_up := f :: !clean_up; 
            Mutex.unlock com_lock;
            sync_events (i+1)
        | None -> 
            let j = i + Random.int (Array.length bev - i) in
            let temp =  bev.(i) in 
            bev.(i) <- bev.(i); bev.(j) <- temp;
            Mutex.unlock com_lock;
            sync_events i in
  sync_events 0

let select evl = sync (choose evl)

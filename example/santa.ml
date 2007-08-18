(* OCaml version of the Santa Claus problem documented in Simon Peython Jones's
   "Beautiful concurrency" paper. This is a _literal_ translation of the Haskell
   version (attached as comment a the end of this file)
*)
module Thread=Cothread
open Stm

type gate = {gt_num:int; gt_left: int tvar}
let new_gate n = 
  new_tvar 0 >>= fun left -> 
    return {gt_num = n; gt_left = left}

let use_gate {gt_left = left} = 
  atom (read_tvar left >>= fun v -> 
              if v > 0 then write_tvar left (v - 1)
              else retry)

let operate_gate {gt_num = num; gt_left = left} = 
  atom (write_tvar left num);
  atom (read_tvar left >>= fun v -> 
              if v > 0 then retry else return ())


type group = {gp_num: int; gp_left: (int * gate * gate) tvar}
let new_group n = atom
  (new_gate n >>= fun g1 ->
     new_gate n >>= fun g2 -> 
       new_tvar (n, g1, g2) >>= fun tv ->
         return {gp_num = n; gp_left = tv})

let join_group {gp_left = left} = atom 
  (read_tvar left >>= fun (n_left, g1, g2) -> 
     if n_left > 0 then 
       write_tvar left (n_left - 1, g1, g2) >> return (g1, g2)
     else retry)

let await_group {gp_num = num; gp_left = left} =
  read_tvar left >>= fun (n_left, g1, g2) ->
    if n_left = 0 then 
      new_gate num >>= fun new_g1 ->
        new_gate num >>= fun new_g2 ->
          write_tvar left (num, new_g1, new_g2) >> return (g1, g2)
    else retry


let rec helper gp id task =
  let in_gate, out_gate = join_group gp in
  use_gate in_gate; task id; flush stdout; use_gate out_gate;
  Thread.delay (Random.float 1.0);
  helper gp id task

let run task (in_gt, out_gt) =  
  Printf.printf "Ho! Ho! Ho! let's %s\n" task; flush stdout;
  operate_gate in_gt;
  operate_gate out_gt

(* Note that IO () in haskell corresponds here to () -> () *)
let choose choices =
  let actions = List.map 
    (fun (stm,act) -> 
       stm >>= fun x -> return (fun () -> act x)) choices in
  let action = match actions with
    | [] -> return (fun () -> ())
    | h::t -> List.fold_left or_else h t in
  atom action

let rec santa elf_gp rein_gp =
  print_endline "----------------------";
  choose [ (await_group rein_gp, run "deliver toys");
           (await_group elf_gp, run "meet in study");
         ] ();
  santa elf_gp rein_gp

let main () = 
  let elf_gp = new_group 3 in
  let _ = Array.init 10 
    (Thread.create 
       (fun i -> helper elf_gp (i + 1) 
          (Printf.printf "Elf %d meeting in the study\n"))) in
  let rein_gp = new_group 9 in
  let _ = Array.init 9 
    (Thread.create 
       (fun i -> helper rein_gp (i + 1) 
          (Printf.printf "Reindeer %d delivering toys\n"))) in
  santa elf_gp rein_gp

let _ = main ()



(* We attach the original Haskell solution below *)

(*

{-# OPTIONS -package stm #-}

module Main where

import Control.Concurrent.STM
import Control.Concurrent
import System.Random

main = do { elf_gp <- newGroup 3
          ; sequence [ elf elf_gp n | n <- [1..10]]

	  ; rein_gp <- newGroup 9
          ; sequence [ reindeer rein_gp n | n <- [1..9]]

	  ; forever (santa elf_gp rein_gp) }
  where
    elf      gp id = forkIO (forever (do { elf1 gp id; randomDelay }))
    reindeer gp id = forkIO (forever (do { reindeer1 gp id; randomDelay }))

santa :: Group -> Group -> IO ()
santa elf_group rein_group
  = do { putStr "----------\n"
       ; choose [(awaitGroup rein_group, run "deliver toys"), 
		 (awaitGroup elf_group,  run "meet in my study")] }
  where
    run :: String -> (Gate,Gate) -> IO ()
    run what (in_gate,out_gate) 
	= do { putStr ("Ho! Ho! Ho! let's " ++ what ++ "\n")
	     ; operateGate in_gate
	     ; operateGate out_gate }

helper1 :: Group -> IO () -> IO ()
helper1 group do_task
  = do { (in_gate, out_gate) <- joinGroup group
       ; useGate in_gate
       ; do_task
       ; useGate out_gate }

elf1, reindeer1 :: Group -> Int -> IO ()
elf1      group id = helper1 group (meetInStudy id)
reindeer1 group id = helper1 group (deliverToys id)


deliverToys id = putStr ("Reindeer " ++ show id ++ " delivering toys\n")
meetInStudy id = putStr ("Elf " ++ show id ++ " meeting in the study\n")


---------------
data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup :: Int -> IO Group
newGroup n = atomically (do { g1 <- newGate n
	        	    ; g2 <- newGate n
	        	    ; tv <- newTVar (n, g1, g2)
        		    ; return (MkGroup n tv) })

joinGroup :: Group -> IO (Gate,Gate)
joinGroup (MkGroup n tv) 
  = atomically (do { (n_left, g1, g2) <- readTVar tv
       		   ; check (n_left > 0) 
       		   ; writeTVar tv (n_left-1, g1, g2)
       		   ; return (g1,g2) })

awaitGroup :: Group -> STM (Gate,Gate)
awaitGroup (MkGroup n tv) 
  = do { (n_left, g1, g2) <- readTVar tv
       ; check (n_left == 0) 
       ; new_g1 <- newGate n
       ; new_g2 <- newGate n
       ; writeTVar tv (n,new_g1,new_g2)
       ; return (g1,g2) }

---------------
data Gate  = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = do { tv <- newTVar 0; return (MkGate n tv) }

useGate :: Gate -> IO ()
useGate (MkGate n tv) 
  = atomically (do { n_left <- readTVar tv
  	           ; check (n_left > 0)
  	           ; writeTVar tv (n_left-1) })

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) 
  = do { atomically (writeTVar tv n)
       ; atomically (do { n_left <- readTVar tv
		        ; check (n_left == 0) }) }

----------------

forever :: IO () -> IO ()
-- Repeatedly perform the action
forever act = do { act; forever act }

randomDelay :: IO ()
-- Delay for a random time between 1 and 1000,000 microseconds
randomDelay = do { waitTime <- getStdRandom (randomR (1, 1000000))
                 ; threadDelay waitTime }

choose :: [(STM a, a -> IO ())] -> IO ()
choose choices = do { to_do <- atomically (foldr1 orElse stm_actions)
		    ; to_do }
  where
    stm_actions :: [STM (IO ())]
    stm_actions = [ do { val <- guard; return (rhs val) }
		  | (guard, rhs) <- choices ] 

*)

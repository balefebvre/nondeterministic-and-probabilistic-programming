(** Testing the Proba monad *)

open Printf
open Proba.Tree

(** Throwing three dices *)

let d6 : int mon = uniform 1 6

let _3d6 : int mon =
  d6 >>= (fun i1 ->
  d6 >>= (fun i2 ->
  d6 >>= (fun i3 -> ret (i1 + i2 + i3))))

let _ =
  printf "--- 3d6\n";
  print_run (fun n -> printf "%d" n) 10 _3d6

(** An observation that modifies a posteriori probabilities.
  We flip two coins and observe that they don't come up both with heads.
  What is the distribution of the two coins? *)

let not_both_true : (bool * bool) mon =
  flip 0.5 >>= fun b1 ->
  flip 0.5 >>= fun b2 ->
  observe (not (b1 && b2)) >>= fun _ ->
  ret (b1, b2)

let _ =
  printf "--- not both true\n";
  print_run (fun (b1, b2) -> printf "%b, %b" b1 b2) 10 not_both_true

(** A probabilistic algorithm that terminates almost always.
  We pick an integer between 0 and 3 inclusive, and if 3 comes out
  we pick again.  The possible results are, therefore, 0, 1 and 2,
  with probability 1/3 each. *)

let rec rand3 () : int mon =
  uniform 0 3 >>= fun n ->
  if n < 3 then ret n else rand3 ()

let _ =
  printf "--- rand3\n";
  print_run (fun n -> printf "%d" n) 10 (rand3 ())

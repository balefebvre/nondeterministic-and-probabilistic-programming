(** Fun with monadic lists *)

open Printf
open Nondet.TreeState

type 'a mlist = 'a mlist_content mon
and 'a mlist_content = Nil | Cons of 'a * 'a mlist

let nil : 'a mlist = ret Nil
let cons (hd: 'a) (tl: 'a mlist) : 'a mlist = ret (Cons(hd, tl))

(** Generating an arbitrary mlist of booleans *)

let any_bmlist = fix (fun any_bmlist ->
  nil ||| cons false any_bmlist ||| cons true any_bmlist)

(** Testing generation and filtering *)

let isnil (l: 'a mlist) : unit mon =
  l >>= function Nil -> ret () | Cons(_, _) -> fail

let issingleton (l: 'a mlist) : unit mon =
  l >>= function Nil -> fail | Cons(hd, tl) -> isnil tl

let _ =
  printf "-- isnil\n";
  print_run (fun () -> printf "OK!") 1000 (isnil any_bmlist);
  printf "-- issingleton\n";
  print_run (fun () -> printf "OK!") 1000 (issingleton any_bmlist)

(** Operations over monadic lists *)

let rec mlist_of_list (l: 'a list) : 'a mlist = failwith "TODO"

let rec list_of_mlist (ml: 'a mlist) : 'a list mon = failwith "TODO"

let rec append (ml1: 'a mlist) (ml2: 'a mlist) : 'a mlist = failwith "TODO"

let rec mlist_eq (ml1: 'a mlist) (ml2: 'a mlist) : unit mon = failwith "TODO"

(** Does there exist two mlists that append to the given mlist? *)

let can_split_mlist (ml: bool mlist) : unit mon = failwith "TODO"

let _ =
  printf "-- can_split_mlist\n";
  print_run (fun () -> printf "OK!") 50
    (can_split_mlist (mlist_of_list [true;true;true;false;false]))

(** Adding memoization to the generation of boolean mlists *)

let any_bmlist () : bool mlist = fixmemo (fun any_bmlist ->
 choice [nil; cons false any_bmlist; cons true any_bmlist])

(** Illustrating the effect of memoization. *)

let nil_mlists =
  let l = any_bmlist() in isnil l >>= fun _ -> list_of_mlist l

let singleton_mlists =
  let l = any_bmlist() in issingleton l >>= fun _ -> list_of_mlist l

(** Printing the results *)

let print_bool_list l =
  let rec print = function
  | [] -> ()
  | [b] -> printf "%b" b
  | b :: l -> printf "%b; " b; print l
  in printf "["; print l; printf "]"

let _ =
  printf "-- nil_mlists\n";
  print_run print_bool_list 1000 nil_mlists;
  printf "-- singleton_mlists\n";
  print_run print_bool_list 1000 singleton_mlists

(** All possible ways to split a mlist into the concatenation of two
  mlists.  Returns the two mlists after conversion to normal lists,
  so that we can print the results. *)

let split_mlist (ml: bool mlist) : (bool list * bool list) mon = failwith "TODO"

(** Printing the results *)

let print_2_bool_list (l1, l2) =
  print_bool_list l1; printf ", "; print_bool_list l2

let _ =
  printf "-- split_mlist\n";
  print_run print_2_bool_list 1000
    (split_mlist (mlist_of_list [true;true;true;false;false]))

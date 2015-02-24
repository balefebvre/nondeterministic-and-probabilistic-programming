(** Reverse execution of list concatenation to obtain all possible ways
    of splitting a list in two halves. *)

open Printf
open Nondet.Tree

let any_bool : bool mon = ret false ||| ret true

let any_blist : bool list mon = fix (fun any_blist -> 
    ret []
|||
    (any_bool >>= fun hd ->
     any_blist >>= fun tl ->
     ret (hd :: tl)))

let split (l: bool list) : (bool list * bool list) mon =
  any_blist >>= fun l1 ->
  any_blist >>= fun l2 ->
  if l1 @ l2 = l then ret (l1, l2) else fail

let print_bool_list l =
  let rec print = function
  | [] -> ()
  | [b] -> printf "%b" b
  | b :: l -> printf "%b; " b; print l
  in printf "["; print l; printf "]"

let print_2_bool_list (l1, l2) =
  print_bool_list l1; printf ", "; print_bool_list l2

let _ =
  print_run print_2_bool_list 30 (split [true;true;true;false;false])

    

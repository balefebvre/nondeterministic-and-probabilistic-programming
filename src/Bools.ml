(** Simple combinatorial exploration of booleans.
    What are the 3 inputs to a full binary adder that generate a carry? *)

open Printf
open Nondet.Tree

let xor (a: bool) (b: bool) = a <> b

let full_adder a b c =
  (xor (xor a b) c, (a && b) || (a && c) || (b && c))

let any_bool : bool mon = ret false ||| ret true

let solution : (bool * bool * bool) mon =
  any_bool >>= fun a ->
  any_bool >>= fun b ->
  any_bool >>= fun c ->
    let (sum, carry) = full_adder a b c in
    if carry then ret (a, b, c) else fail

let print_3_bool (a, b, c) = printf "%b, %b, %b" a b c

let _ =
  print_run print_3_bool 20 solution

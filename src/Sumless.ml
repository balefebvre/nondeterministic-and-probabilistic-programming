(** What are pairs of integers whose sum is at most 5? *)

open Printf
open Nondet.Tree

(** [any_int n] nondeterministically generate an integer greater than or
    equal to [n]. *)

let any_int : int -> int mon =
  fixparam (fun f i -> ret i ||| f (i + 1))

let sum_less_than_5 =
  any_int 0 >>= (fun i1 ->
  any_int 0 >>= (fun i2 ->
  if i1 + i2 <= 5 then ret(i1, i2) else fail))

let print_2_int (i1, i2) = printf "%d, %d" i1 i2

let _ =
  print_run print_2_int 30 sum_less_than_5

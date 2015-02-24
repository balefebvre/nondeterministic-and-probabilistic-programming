(** Testing memoization on non-recursive data structures *)

open Printf
open Nondet.TreeState

let any_bool : bool mon = ret false ||| ret true

let filter_true (bm: bool mon) : bool mon =
  bm >>= fun bv -> if bv then bm else fail

let filter_both_true (bm1: bool mon) (bm2: bool mon): (bool * bool) mon =
  filter_true bm1 >>= fun bv1 ->
  filter_true bm2 >>= fun bv2 -> ret (bv1, bv2)

let print_bool b = printf "%b" b
let print_2_bool (b1, b2) = printf "%b,%b" b1 b2

let _ =
  printf "-- filter_true without memo\n";
  print_run print_bool 20 (filter_true any_bool);
  printf "-- filter_true with memo\n";
  print_run print_bool 20 (filter_true (memo any_bool));
  printf "-- filter_both_true without memo\n";
  print_run print_2_bool 20 (filter_both_true any_bool any_bool);
  printf "-- filter_true with 2 memo\n";
  print_run print_2_bool 20 (filter_both_true (memo any_bool) (memo any_bool));
  printf "-- filter_true with 1 memo\n";
  print_run print_2_bool 20 (filter_both_true (memo any_bool) any_bool)

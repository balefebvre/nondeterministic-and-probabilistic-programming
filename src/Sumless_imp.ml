(** Testing mutable references in nondeterministic computations  *)

open Printf
open Nondet.TreeState

(** [incr_ref l n] nondeterministically assign [n], [n+1], [n+2], ...
    to the integer reference [r]. *)

let incr_ref (r: int ref) : int -> unit mon =
  fixparam (fun incr_ref i -> setref r i ||| incr_ref (i + 1))

let sum_less_than_5 =
  let r1 : int ref = newref()
  and r2 : int ref = newref() in
  incr_ref r1 0 >>= fun _ ->
  incr_ref r2 0 >>= fun _ ->
  getref r1 >>= fun i1 ->
  getref r2 >>= fun i2 ->
  if i1 + i2 <= 5 then ret(i1, i2) else fail

let print_2_int (i1, i2) = printf "%d, %d" i1 i2

let _ =
  print_run print_2_int 30 sum_less_than_5

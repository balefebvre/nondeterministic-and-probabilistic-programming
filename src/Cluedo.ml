(* A game of Bayesian Cluedo *)

open Printf
open Proba.Tree

type suspect = Alice | Bob

type weapon = Gun | Pipe

let whodunnit : suspect mon = failwith "TODO"

let print_suspect = function
  | Alice -> printf "Alice"
  | Bob   -> printf "Bob"

let _ =
  print_run print_suspect 20 whodunnit


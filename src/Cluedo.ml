(* A game of Bayesian Cluedo *)

open Printf
open Proba.Tree

type suspect = Alice | Bob

type weapon = Gun | Pipe

let whodunnit : suspect mon =
  choose 0.3 (ret Alice) (ret Bob) >>=
    (fun suspect ->
      let p =
	match suspect with
	|Alice -> 0.03
	| Bob -> 0.8
      in
      choose p (ret Gun) (ret Pipe) >>=
      (fun weapon ->
	match weapon with
	|Gun -> fail
	| Pipe -> ret (suspect))
    )

let print_suspect = function
  | Alice -> printf "Alice"
  | Bob   -> printf "Bob"

let _ =
  print_run print_suspect 20 whodunnit

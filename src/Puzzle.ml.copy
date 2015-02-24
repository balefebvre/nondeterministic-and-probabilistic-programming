(** The bridge crossing puzzle *)

open Nondet.Naive   (* or:  open Nondet.Tree *)

type band_member = Bono | Edge | Adam | Larry

(** The actions that can be performed: *)
type action =
  | Forth of band_member * band_member  (** two persons walk forth on the bridge *)
  | Back of band_member                 (** one person walks back on the bridge *)

(** A trace of execution is the list of actions performed. *)
type trace = action list

(** This is the time it takes each person to cross the bridge. *)
let walking_time = function
  | Bono -> 1
  | Edge -> 2
  | Adam -> 5
  | Larry -> 10

(** A handy nondeterministic function: it takes a list and returns
  all the ways to decompose it into one element and the list of the
  other elements. *)

let rec take_one (l: 'a list) : ('a * 'a list) mon = failwith "TODO"

(** Likewise, but decomposes the list in two distinct elements and
  the list of the other elements. *)

let rec take_two (l: 'a list) : ('a * 'a * 'a list) mon = failwith "TODO"

(** The solution to the puzzle! *)

let solution: trace mon = failwith "TODO"

(** Printing the solution *)

open Printf

let name_of = function
  | Bono -> "Bono"
  | Edge -> "Edge"
  | Adam -> "Adam"
  | Larry -> "Larry"

let print_action = function
  | Forth(x, y) -> printf "%s and %s go forth" (name_of x) (name_of y)
  | Back(x) -> printf "%s goes back" (name_of x)

let print_trace t =
  List.iter (fun e -> print_action e; printf "; ") t

let _ =
  print_run print_trace 50 solution

  




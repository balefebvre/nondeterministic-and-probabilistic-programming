(** The bridge crossing puzzle *)

(* open Nondet.Naive *)
(* or *)
open Nondet.Tree

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

let rec take_one (l: 'a list) : ('a * 'a list) mon =
  match l with
  | [] ->
    failwith "no decomposition of the list into one elements and the list of the other elements"
  | [x] ->
    ret (x, [])
  | hd :: tl ->
    ret (hd, tl) ||| (take_one tl >>= (fun (x, l) -> ret (x, hd :: l)))

(** Likewise, but decomposes the list in two distinct elements and
  the list of the other elements. *)

let rec take_two (l: 'a list) : ('a * 'a * 'a list) mon =
  match l with
  | [] | [_] ->
    failwith "no decomposition of the list into two elements and the list of the other elements"
  | [x1; x2] ->
    ret (x1, x2, [])
  | [x1; x2; x3] ->
    ret (x1, x2, [x3]) ||| ret (x1, x3, [x2]) ||| ret (x2, x3, [x1])
  | hd :: tl ->
    (take_one tl >>= (fun (x, l) -> ret (hd, x, l)))
    ||| (take_two tl >>= (fun (x1, x2, l) -> ret (x1, x2, hd :: l)))

(** The solution to the puzzle! *)

let solution: trace mon =
  (** TODO: check if there is no other simple way to implement the solution... **)
  let rec forth time side1 side2 =
    match side1 with
    | [] | [_] -> failwith "no solution"
    | _ ->
      (take_two side1) >>= (fun (x1, x2, l) -> back (time - max (walking_time x1) (walking_time x2)) l (x1 :: x2 :: side2) >>= (fun l -> ret (Forth (x1, x2) :: l)))
  and back time side1 side2 =
    if time < 0
    then fail
    else
      match side1 with
      | [] -> ret []
      | _ -> (take_one side2) >>= (fun (x, l) -> forth (time - walking_time x)(x :: side1) l >>= (fun l -> ret (Back x :: l)))
  in
  let time = 17 in
  let side1 = [Bono; Edge; Adam; Larry] in
  let side2 = [] in
  forth time side1 side2

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

  




(** {1 Monads for probability distributions} *)

open Printf

(** {2 The signature of the monad for probability distributions} *)

type prob = float

type 'a distribution = ('a * prob) list

module type PROBA = sig

  type 'a mon

  val ret: 'a -> 'a mon
  val bind: 'a mon -> ('a -> 'b mon) -> 'b mon
  val (>>=): 'a mon -> ('a -> 'b mon) -> 'b mon

  val distr: 'a distribution -> 'a mon
    (** [distr d] chooses one value among zero, one or several,
        with the probabilities indicated in the distribution [d]. *)

  val flip: prob -> bool mon
    (** [flip p] returns the boolean [true] with probability [p]
        and [false] with probability [1-p]. *)

  val uniform: int -> int -> int mon
    (** [uniform lo hi] returns an integer between [lo] and [hi]
        included, with uniform probability. *)

  val choose: prob -> 'a mon -> 'a mon -> 'a mon
    (** [choose p a b] executes like [a] with probability [p]
        and like [b] with probability [1-p]. *)

  val fail: 'a mon
    (** Failure *)

  val observe: bool -> unit mon
    (* [observe b] continues (returning [()]) if [b] is [true]
       and fails if [b] is false. *)

  val run: int -> 'a mon -> 'a distribution * prob
    (* [run maxdepth m] explores the monadic computation [m]
       to maximal depth [m].  It returns a distribution of
       possible values, plus a combined probability for the
       parts of the monadic computation that were not explored
       because they exceed the maximal depth. *)

  val print_run: ('a -> unit) -> int -> 'a mon -> unit
    (* [print_run f maxdepth m] explores [m] like [run maxdepth m],
       then prints the resulting distribution using [f] to print
       individual values. *)
end

(** {2 Auxiliary functions for implementing monads} *)

(** Auxiliary for printing the results of a run *)

let print_run_aux (f: 'a -> unit) ((res, unknown): 'a distribution * prob) =
  List.iter
    (fun (x, p) -> printf "%10g: " p; f x; printf "\n")
    res;
  if unknown > 0.0 then
    printf "%10g: unknown\n" unknown

(** Auxiliary for removing duplicates from a distribution,
    accumulating their combined probabilities. *)

let remove_dups (l: 'a distribution) : 'a distribution =
  let rec remove l accu =
    match l with
    | [] -> accu
    | [xp] -> xp :: accu
    | (x1, p1 as xp1) :: ((x2, p2) :: l2 as l1) ->
        if x1 = x2
        then remove ((x1, p1 +. p2) :: l2) accu
        else remove l1 (xp1 :: accu)
  in List.rev (remove (List.sort (fun (x1,p1) (x2,p2) -> compare x1 x2) l) [])

(** Auxiliary to normalize the probabilities in a distribution
    so that they sum to 1. *)

let normalize ((res, unknown): 'a distribution * prob) =
  let total = 
    List.fold_left (fun tot (x, p) -> tot +. p) unknown res in
  (List.map (fun (x, p) -> (x, p /. total)) res, unknown /. total)

(** {2 The lazy probabilistic choice tree monad} *)

module Tree : PROBA = struct

  type 'a mon = unit -> 'a case distribution
  and 'a case = Val of 'a | Susp of 'a mon

  let ret (x: 'a) : 'a mon = failwith "TODO"

  let rec bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon = failwith "TODO"

  let (>>=) = bind

  let fail : 'a mon = failwith "TODO"

  let observe (b: bool) : unit mon =
    if b then ret () else fail

  let distr (d: 'a distribution) : 'a mon = failwith "TODO"

  let flip (p: prob) : bool mon =
    distr [(true, p); (false, 1.0 -. p)]

  let uniform (lo: int) (hi: int) : int mon = failwith "TODO"

  let choose (p: prob) (a: 'a mon) (b: 'a mon) : 'a mon = failwith "TODO"

  let flatten (maxdepth: int) (m: 'a mon) : 'a case distribution = failwith "TODO"

  let run (maxdepth: int) (m: 'a mon) : 'a distribution * prob = failwith "TODO"

  let print_run f depth m = print_run_aux f (run depth m)

end



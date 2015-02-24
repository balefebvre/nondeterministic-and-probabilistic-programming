(** {1 Monads for nondeterminsm} *)

open Printf

(** {2 The signature of the monad} *)

module type NONDET = sig

  type 'a mon
    (** The type of monadic computations producing values of type ['a]. *)

  val ret: 'a -> 'a mon
    (** The standard "return" operation of the monad. *)
  val bind: 'a mon -> ('a -> 'b mon) -> 'b mon
    (** The standard "bind" operation of the monad. *)

  val (>>=): 'a mon -> ('a -> 'b mon) -> 'b mon
    (** A convenient infix notation for [bind].
        You can write [a >>= fun x -> b] instead of [bind a (fun x -> b)]. *)

  val choice: 'a mon list -> 'a mon
    (** Nondeterministic choice between zero, one or several possibilities.
        [choice al] nondeterministically choose one of the ['a mon]
        monadic computations given in the list [al]. *)

  val fail: 'a mon
    (** Failure.
        [fail] is equivalent to [choice []], i.e. choice among zero
        possibilities. *)

  val either: 'a mon -> 'a mon -> 'a mon
    (** Choice between two possibilities.
        [either a b] is equivalent to [choice [a;b]]. *)

  val (|||):  'a mon -> 'a mon -> 'a mon
    (** A convenient infix notation for [either].
        You can write [a ||| b] instead of [either a b]. *)

  val run: int -> 'a mon -> 'a list * bool
    (** [run maxdepth m] executes the nondeterministic computation
        contained in [m] and produces the list of possible values
        (first component of the returned pair).
        The first parameter [maxdepth] limits the depth of exploration
        of the computation [m].  This limited depth ensures that [run]
        terminates, but can cause it to miss some possible values.
        In this case, the second component of the pair returned by
        [run] will be [false], meaning "not exhaustive".
        If all possible values are found, [true], meaning "exhaustive",
        is returned instead. *)

  val print_run: ('a -> unit) -> int -> 'a mon -> unit
    (** [print_run f maxdepth m] executes the nondeterministic computation
        [m] just like [run maxdepth m] does, then prints the resulting
        values.  The function [f] is a printing function for values of
        type ['a].  It is called on every possible value of [m]. *)

  val fix: ('a mon -> 'a mon) -> 'a mon
    (** Monadic fixpoint combinator.  
[[          let x = fix (fun x -> e)      ]]
        behaves like
[[          let rec x = e                 ]]
        but works for arbitrary expressions [e] of monadic type. *)

  val fixparam: (('a -> 'b mon) -> ('a -> 'b mon)) -> 'a -> 'b mon
    (** Monadic fixpoint combinator for one-parameter functions.  
[[          let f = fixparam (fun f x -> e)      ]]
        behaves like
[[          let rec f x = e                      ]]
        but works for arbitrary expressions [e] of monadic type,
        and is not strict in [e]. *)
end

(** {2 Auxiliary functions for implementing monads} *)

(** Auxiliary for printing the results of a run. *)

let print_run_aux (f: 'a -> unit) ((res, exhaustive): 'a list * bool) =
  List.iteri
   (fun i x -> printf "Result #%d: " (i + 1); f x; printf "\n")
   res;
 if exhaustive then
   printf "Exhaustive -- there are no other results\n"
 else
   printf "Partial -- there are other possible results\n"

(** Auxiliary for removing duplicates in a list.  This is
    [List.sort_uniq compare] in OCaml 4.02. *)

let sort_uniq l =
  let rec uniq l accu =
    match l with
    | [] -> accu
    | [h] -> h :: accu
    | h1 :: (h2 :: _ as l1) ->
        if h1 = h2 then uniq l1 accu else uniq l1 (h1 :: accu)
  in List.rev (uniq (List.sort compare l) [])

(** {2 The naive list monad} *)

(** This is the implementation of nondeterminism as a list of possible 
    values already seen in the lectures. *)

module Naive : NONDET = struct

  type 'a mon = 'a list

(** Monad operations *)

  let ret (x: 'a) : 'a mon = [x]

  let rec bind (m: 'a mon) (f: 'a -> 'b mon) : 'b mon =
    match m with
    | [] -> []
    | h :: t -> f h @ bind t f

  let (>>=) = bind

(** Nondeterminism *)

  let choice (al: 'a mon list) : 'a mon = List.flatten al

  let fail : 'a mon = []

  let either (a: 'a mon) (b: 'a mon) : 'a mon = a @ b

  let (|||) = either

(** For fixpoint operators, there is no usable [fix] combinator in
    this monad, because of the eager evaluation of Caml's lists.
    [fixparam] is equivalent to a Caml recursive function definition. *)

  let fix (f: 'a mon -> 'a mon) : 'a mon =
    failwith "no usable 'fix' in the Naive monad"

  let fixparam (f: ('a -> 'b mon) -> ('a -> 'b mon)) : 'a -> 'b mon =
    let rec res x = f res x in res

(** Running a monadic computation is trivial because all possible choices
    have been explored already by Caml's eager evaluation strategy.
    So, we just sort and remove duplicates in the list of results.
    For the same reason, the returned list of results is always
    exhaustive. *)
  let run (maxdepth: int) (m: 'a mon) : 'a list * bool = (sort_uniq m, true)

(** Run then print the results. *)
  let print_run f depth m = print_run_aux f (run depth m)

end

(** {2 The lazy choice tree monad} *)

module Tree : NONDET = struct

  type 'a mon = unit -> 'a case list
  and 'a case = Val of 'a | Susp of 'a mon

(** Monad operations *)

  let ret (x: 'a): 'a mon = (fun () -> [Val x])

  let rec bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon =
    fun () ->
      List.map
	(fun x -> match x with
	 | Val a -> Susp (f a)
	 | Susp n -> Susp (bind n f))
	(m ())

  let (>>=) = bind

(** Nondeterminism *)

  let choice (al: 'a mon list) : 'a mon = (fun () -> List.map (fun m -> Susp m) al)

  let fail : 'a mon = (fun () -> [])

  let either (a: 'a mon) (b: 'a mon): 'a mon = (fun () -> [Susp a; Susp b])

  let (|||) = either

(** The [flatten] function below is the core of the [run] operation.
    [flatten maxdepth m] forces the evaluation of suspensions [Susp]
    contained in the choice tree [m] up to depth [maxdepth]. 
    See project description for examples. *)

  let flatten (maxdepth: int) (m: 'a mon) : 'a case list =
    let rec search_case (depth: int) (c: 'a case) : 'a case list =
      match c with
      | Val x -> [Val x]
      | Susp m -> search (depth - 1) (m ())
    and search (depth: int) (l: 'a case list) : 'a case list =
      match (depth, l) with
      | (0, l) -> l
      | (n, []) -> []
      | (n, hd :: tl) -> search_case n hd @ search n tl
    in
    let l = m () in
    search maxdepth l

(** [run maxdepth m] first calls [flatten maxdepth m] to explore
    the choice tree [m] to bounded depth [maxdepth].  Then it
    separates [Val] and [Susp] cases in the result of
    [flatten maxdepth m] to produce the expected result. *)

  let run (maxdepth: int) (m: 'a mon) : 'a list * bool =
    let rec process (l: 'a case list) (accu: 'a list) (exha: bool) : 'a list * bool =
      match l with
      | [] -> (accu, exha)
      | (Val x) :: tl -> process tl (x :: accu) exha
      | (Susp _) :: tl -> process tl accu false
    in
    let l = flatten maxdepth m in
    process l [] true

  let print_run f depth m = print_run_aux f (run depth m)

(** Fixpoint operators *)

  let fix (f: 'a mon -> 'a mon) : 'a mon =
    let rec res : 'a mon = (fun () -> f res ()) in res

  let fixparam (f: ('a -> 'b mon) -> ('a -> 'b mon)) : 'a -> 'b mon =
    let rec res (x: 'a) : 'b mon = (fun () -> f res x ()) in res
end

(** {2 Adding local state to the choice tree monad} *)

(** Signature for the additional operations. *)

module type NONDET_WITH_STATE = sig
  include NONDET

  type 'a ref
    (** The type of references to locations containing values of type ['a]. *)

  val newref: unit -> 'a ref
    (** Creates a fresh reference, without initializing it. *)

  val getref: 'a ref -> 'a mon
    (** Return the current value of the given reference.
        Fail if the reference was never initialized. *)

  val setref: 'a ref -> 'a -> unit mon
    (** [setref r v] assigns value [v] to reference [r]. *)

  val memo: 'a mon -> 'a mon
    (** Memoization of a monadic computation.  See the last section
        of the project description. *)

  val fixmemo: ('a mon -> 'a mon) -> 'a mon
    (** Memoizing fixpoint.  See the last section of the project
        description. *)
end

(** Implementation of the state-and-choice-tree monad. *)

module TreeState : NONDET_WITH_STATE = struct

  type 'a mon = Store.t -> ('a case * Store.t) list
  and 'a case = Val of 'a | Susp of 'a mon

(** Monad operations *)

  let ret (x: 'a): 'a mon = failwith "TODO"

  let rec bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon = failwith "TODO"

  let (>>=) = bind

(** Nondeterminism *)

  let choice (al: 'a mon list) : 'a mon = failwith "TODO"

  let fail : 'a mon = fun s -> failwith "TODO"

  let either (a: 'a mon) (b: 'a mon): 'a mon = failwith "TODO"

  let (|||) = either

(** Operations over the store *)

  type 'a ref = 'a Store.loc

  let newref: unit -> 'a ref = Store.newloc
  
  let getref (l: 'a ref) : 'a mon = failwith "TODO"

  let setref (l: 'a ref) (v: 'a) : unit mon = failwith "TODO"

(** Running a monadic computation *)

  let flatten maxdepth (m: 'a mon) (s: Store.t) : ('a case * Store.t) list = failwith "TODO"

  let run (maxdepth: int) (m: 'a mon) : 'a list * bool = failwith "TODO"

  let print_run f depth m = print_run_aux f (run depth m)

(** Fixpoint operators *)

  let fix (f: 'a mon -> 'a mon) : 'a mon = failwith "TODO"

  let fixparam (f: ('a -> 'b mon) -> ('a -> 'b mon)) : 'a -> 'b mon = failwith "TODO"

(** Memoization of monadic computations.  See the last section of the
    project description.  Do not try to implement them before you
    attack this last section. *)

  let memo (a: 'a mon) : 'a mon = failwith "TODO"

  let rec fixmemo (f: 'a mon -> 'a mon) : 'a mon = failwith "TODO"

end 
 

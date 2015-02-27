(** From type checking to type inference and term inference,
    using a monadic representation of types and memoization *)

open Printf
open Nondet.TreeState

(** The simply-typed, implicitly-typed lambda-calculus *)

type var = string

type term =
  | Const of int
  | Var of var
  | Lam of var * term
  | App of term * term

type typ =
  | Int
  | Fun of typ * typ

(* Monadic representation of types *)

type mtyp = mtyp_descr mon
and mtyp_descr = Mint | Mfun of mtyp * mtyp

let mint : mtyp = ret Mint
let mfun (t1: mtyp) (t2: mtyp) = ret (Mfun(t1, t2))

(* Generation of all types *)

(* Incorrect definition of 'any_mtyp' since mtyp is of the form
   mint of t1 -> t2 where t1 = t2

let any_mtyp() : mtyp =
  fixmemo (fun any_mtyp -> mint ||| mfun any_mtyp any_mtyp)
*)

let any_mtyp() : mtyp =
  fixmemo
    (fun any_mtyp1 ->
      mint
      ||| fixmemo
	  (fun any_mtyp2 ->
	    mfun any_mtyp1 mint
            ||| mfun any_mtyp1 any_mtyp2))

(* Conversions between mtyp and typ *)

let rec mtyp_of_typ = function
  | Int -> mint
  | Fun(t1, t2) -> mfun (mtyp_of_typ t1) (mtyp_of_typ t2)

let rec typ_of_mtyp (mt: mtyp) : typ mon =
  mt >>= function
        | Mint -> ret Int
        | Mfun(mt1, mt2) ->
            typ_of_mtyp mt1 >>= fun t1 ->
            typ_of_mtyp mt2 >>= fun t2 -> ret (Fun(t1, t2))

(** Equality between mtyp *)

let rec eq_mtyp (mt1: mtyp) (mt2: mtyp) : unit mon =
  mt1 >>=
    (function
    | Mint ->
      mt2 >>=
	(function
	| Mint -> ret ()
	| _ -> fail)
    | Mfun (t11, t12) ->
      mt2 >>=
	(function
	| Mfun (t21, t22) -> eq_mtyp t11 t21 >>= (function _ -> eq_mtyp t12 t22)
	| _ -> fail))

(** Typing environments: lists of (variable : type) facts. *)

type typenv = (var * mtyp) list 

let type_of_var (env: typenv) (v: var) : mtyp option =
  try Some(List.assoc v env) with Not_found -> None

(** Type checking *)

(** [typeof env a] computes (nondeterministically) the possible type(s)
  for term [a] in environment [env]. *)

let rec typeof (env: typenv) (a: term) : mtyp =
  match a with
  | Const _ -> mint
  | Var v -> (function Some mt -> mt | None -> fail) (type_of_var env v)
  | Lam (v, b) ->
    let mt = any_mtyp () in
    mfun mt (typeof ((v, mt) :: env) b)
  | App (b, c) ->
    let mtc = typeof env c in
    mtc >>=
      (function _ ->
	typeof env b >>=
	  (function
	  | Mfun (mtb1, mtb2) -> eq_mtyp mtc mtb1 >>= (function _ -> mtb2)
	  | _ -> fail))

let types_of_closed_term (a: term) : typ mon = 
  typ_of_mtyp (typeof [] a)

(** Printing of types *)

let rec print_typ = function
  | Fun(t1, t2) ->
      print_typ_0 t1; printf "->"; print_typ t2
  | t ->
      print_typ_0 t

and print_typ_0 = function
  | Int ->
      printf "int"
  | t ->
      printf "("; print_typ t; printf ")"

(** What are the types of [(\x.x) 42] ? *)

let ex1 = types_of_closed_term (App(Lam("x", Var "x"), Const 0))

let _ = print_run print_typ 20 ex1

(** What are the types of [(\x.\y.x) 0] ? *)

let ex2 = types_of_closed_term (App(Lam("x", Lam("y", Var "x")), Const 0))

let _ = print_run print_typ 50 ex2

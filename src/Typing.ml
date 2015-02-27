(** From type checking to type inference and term inference *)

open Printf
open Nondet.Tree

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

(** Enumeration of types *)

let any_typ : typ mon = fix (fun any_typ ->
    ret Int
|||
    (any_typ >>= fun t1 ->
     any_typ >>= fun t2 ->
     ret (Fun(t1, t2))))

(** Typing environments: lists of (variable : type) facts. *)

type typenv = (var * typ) list 

let type_of_var (env: typenv) (v: var) : typ option =
  try Some(List.assoc v env) with Not_found -> None

(** Type checking *)

(** [typeof env a] computes (nondeterministically) the possible type(s)
  for term [a] in environment [env]. *)

let rec typeof (env: typenv) (a: term) : typ mon =
  match a with
  | Const n -> (ret Int)
  | Var v -> (function Some t -> (ret t) | None -> fail) (type_of_var env v)
  | Lam (v, b) -> any_typ >>= (fun tv -> typeof ((v, tv) :: env) b >>= (fun tb -> (ret (Fun (tv, tb)))))
  | App (b, c) -> typeof env b >>= (function Int -> fail | Fun (tb1, tb2) -> checktype env c tb1 >>= (function _ -> (ret tb2)))

(** [checktype env a t] returns [()] if term [a] has type [t] in
  environment [env], and fails otherwise. *)

and checktype (env: typenv) (a: term) (t: typ) : unit mon =
  match (a, t) with
  | (Const n, Int) -> (ret ())
  | (Var v, t) when (type_of_var env v = Some t) -> (ret ())
  | (Lam (v, b), Fun (tv, tb)) -> checktype ((v, tv) :: env) b tb
  | (App (b, c), t) -> typeof env c >>= (fun tc -> checktype env b (Fun (tc, t)))
  | _ -> fail

let types_of_closed_term a = typeof [] a

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

let _ = print_run print_typ 20 ex2

(** Enumeration of terms *)

let rec any_int_below (n: int) : int mon =
  if n <= 0 then fail else ret (n-1) ||| any_int_below (n-1)

let var_x (n: int) : var = "x" ^ string_of_int n

(** [any_term n] generates terms whose free variables are among
    [var_x 0] ... [var_x (n-1)]. *)

let any_term : int -> term mon = fixparam (fun any_term n ->
    ret (Const 42)
|||
    (any_int_below n >>= (fun v -> ret (Var (var_x v))))
|||
    (any_term (n + 1) >>= (fun a -> ret (Lam (var_x n, a))))
|||
    (any_term n >>= (fun a -> any_term n >>= (fun b -> ret (App (a, b)))))
)

(** Generate closed terms that have type [t]. *)

let closed_terms_of_type t : term mon =
  any_term 0 >>= (fun a -> checktype [] a t >>= (fun () -> (ret a)))

(* Printing of terms *)

let rec print_term = function
  | Lam(x, a) ->
      printf "\\%s. " x; print_term a
  | a ->
      print_term_1 a

and print_term_1 = function
  | App(a, b) ->
      print_term_1 a; printf " "; print_term_0 b
  | a ->
      print_term_0 a

and print_term_0 = function
  | Const n ->
      printf "%d" n
  | Var v ->
      printf "%s" v
  | a ->
      printf "("; print_term a; printf ")"

(* Examples *)

let ex3 = closed_terms_of_type (Fun(Int, Int))

let _ = print_run print_term 22 ex3

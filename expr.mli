(*
  Abstract syntax for Spark expressions
*)



type varid = string ;;

type aexpr =
  | Var of varid
  | Int of int
  | Neg of aexpr
  | Plus of aexpr * aexpr
  | Times of aexpr * aexpr
  | Pow of aexpr * aexpr ;;


type bexpr =
  | Bool of bool
  | Equals of aexpr * aexpr
  | LessThan of aexpr * aexpr


type lexpr =
  (* \x. e *)
  | Lam of varid * expr
  (* e1 e2 *)
  | App of expr * expr
  (* let x = e1 in e2 *)
  | Let of varid * expr * expr ;;


type expr =
  | aexpr
  | bexpr
  | lexpr ;;

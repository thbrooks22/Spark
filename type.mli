(*
  Type system for Spark
*)

module type Tp =
sig
  type gammaset
  type deltaset

  type tp =
    | Int
    | Bool
    | String
    (* {l1 : tp1, ..., ln : tpn} *)
    | Record of (string * tp) list
    (* f : tp1 -> tp2 *)
    | To of tp * tp
    (* left/right-injection sum type *)
    | Sum of tp * tp
    (* universal type for parametric polymorphism *)
    | Forall of string * tp
    (* type variable for parametric polymorphism*)
    | Typevar of string


  (*
    ok : Method to check if a type variable is bound in a set.
  *)
  val ok : deltaset -> string -> bool

  (*
    new_type_var : Method to generate a new type variable distinct from every
      element in a set.
  *)
  val new_type_var : deltaset -> string

  (*
    type_expr : Method to type an expression in given environment with a set of
      bound type variables.
  *)
  val type_expr : gammaset -> deltaset -> Expr.expr -> tp
end

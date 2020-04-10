module Grammar =
  struct
    module type Type =
      sig
        type expr
        type gammaset
        type deltaset
        type tp =
          | Unit
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


        val empty_type_context : gammaset
        val empty_typevar_set : deltaset

        (*
          ok : Method to check if a type variable is bound in a set.
        *)
        val ok : tp -> deltaset -> bool

        (*
          new_type_var : Method to generate a new type variable distinct from every
            element in a set.
        *)
        val new_type_var : deltaset -> string

        (*
          type_expr : Method to type an expression in given environment with a set of
            bound type variables.
        *)
        val type_expr : gammaset -> deltaset -> expr -> tp
      end


      module type Expr =
        sig
          type tp
          type expr =
            | U
            | Var of string
            | Int of int
            | Neg of expr
            | Plus of expr * expr
            | Times of expr * expr
            | Pow of expr * expr
            | Bool of bool
            | Equals of expr * expr
            | Lessthan of expr * expr
            (* \x. e *)
            | Lam of (string * tp) * expr
            (* e1 e2 *)
            | App of expr * expr
            (* let x = e1 in e2 *)
            | Let of string * expr * expr
        end
  end

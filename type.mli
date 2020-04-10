(*
  Type system for Spark
*)


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
  | Typevar of string ;;


module Gamma =
  struct
    type t = string * tp
    let compare a b = String.compare (fst a) (fst b)
  end ;;


module Delta =
  struct
    type t = string
    let compare = String.compare
  end ;;



(*
  GammaSet : A typing environment. Maps bound program variables to their types.
    Implemented as a set of string * tp tuples.
*)
module GammaSet = Set.Make(Gamma) ;;
type gammaset = GammaSet.t ;;

(*
  DeltaSet : A set of bound type variables.
*)
module DeltaSet = Set.Make(Delta) ;;
type deltaset = DeltaSet.t ;;

module StringSet = Set.Make(String) ;;
type stringset = StringSet.t ;;


val new_type_var :

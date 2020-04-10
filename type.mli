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



module GammaSet = Set.Make(Gamma) ;;
module DeltaSet = Set.Make(Delta) ;;

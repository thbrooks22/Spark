open Grammar


exception UnboundVarException of string
exception UnboundTypeVarException of string
exception TypeException of string

module rec Type : Grammar.Type
  with type expr := Expr.expr and
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
    | Typevar of string =
  struct
    module Gamma =
      struct
        type t = string * tp
        let compare a b = String.compare (fst a) (fst b)
      end ;;

    (*
      GammaSet : A typing environment. Maps bound program variables to their types.
        Implemented as a set of string * tp tuples.
    *)
    module GammaSet = Set.Make(Gamma) ;;
    type gammaset = GammaSet.t ;;
    let empty_type_context = GammaSet.empty ;;
    (* type_of : Method to find the type of variable x in context gamma. *)
    let type_of (gamma : gammaset) (x : string) : tp option =
      try Some (snd (GammaSet.find (x, Unit) gamma))
      with Not_found -> None ;;

    (*
      DeltaSet : A set of bound type variables.
    *)
    module DeltaSet = Set.Make(String) ;;
    type deltaset = DeltaSet.t ;;
    let empty_typevar_set = GammaSet.empty ;;


    let rec ok (tau : tp) (delta : deltaset) : bool =
      match tp with
      | Unit
      | Int
      | Bool
      | String -> true
      | Record lst ->
        (match lst with
        | [] -> true
        | (str, tau') :: tl -> ok tau' delta && ok (Record tl) delta)
      | To (tau1, tau2)
      | Sum (tau1, tau2) ->
        ok tau1 delta && ok tau2 delta
      | Forall (x, tau') ->
        ok tau' (DeltaSet.add x delta)
      | Typevar x -> DeltaSet.mem x delta ;;


    let new_type_var (delta : deltaset) : string =
      let rec new_type_var' (delta' : deltaset) (str : string) : string =
        if ok str delta'
          then new_type_var' delta' (str ^ "0")
        else str
        in
      new_type_var' delta "X" ;;


    let rec type_check (gamma : gammaset) (delta : deltaset) (e : expr) : tp =
      match e with
      | U -> Unit
      | Var x ->
        (match type_of gamma x with
        | Some tau ->
          if ok tau delta then tau
          else raise (UnboundTypeVarException "Type variable binding error.")
        | None -> raise (UnboundVarException "Variable " ^ x ^ " unbound."))
      | Int _ -> Int
      | Neg e' ->
        if (type_check gamma delta e') == Int then Int
        else raise (TypeException "Type Int expected.")
      | Plus (e1, e2)
      | Times (e1, e2)
      | Pow (e1, e2) ->
        if (type_check gamma delta e1) == Int && (type_check gamma delta e2) == Int
          then Int
        else raise (TypeException "Type Int expected.")
      | Bool _ -> Bool
      | Equals (e1, e2)
      | Lessthan (e1, e2) ->
        if (type_check gamma delta e1) == Int && (type_check gamma delta e2) == Int
          then Bool
        else raise (TypeException "Type Int expected.")
      | Lam ((x, tau), e) ->


  end

and Expr : Grammar.Expr
  with type tp := Type.tp and
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
    | Let of string * expr * expr =
  struct

  end

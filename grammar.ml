open Grammar


exception UnboundVarException of string
exception UnboundTypeVarException of string
exception ApplicationException of string
exception TypeApplicationException of string
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
    (* *************************************************************************
      GammaSet : A typing environment. Maps bound program variables to their types.
        Implemented as a set of string * tp tuples.
    ************************************************************************* *)
    module GammaSet = Set.Make(
                              struct
                                type t = string * tp
                                let compare a b = String.compare (fst a) (fst b)
                              end
                      ) ;;
    type gammaset = GammaSet.t ;;

    let empty_type_context = GammaSet.empty ;;

    (* type_of : Method to find the type of variable x in context gamma. *)
    let type_of (gamma : gammaset) (x : string) : tp option =
      try Some (snd (GammaSet.find (x, Unit) gamma))
      with Not_found -> None ;;

    (* append : Method to expand/edit type context with variable x of type tau. *)
    let append (gamma : gammaset) (x : string) (tau : tp): gammaset =
      let gamma' = GammaSet.remove (x, tau) gamma in
      GammaSet.add (x, tau) gamma' ;;

    (* *************************************************************************
    ************************************************************************* *)


    (* ************************************************************************
      DeltaSet : A set of bound type variables.
    ************************************************************************* *)
    module DeltaSet = Set.Make(String) ;;
    type deltaset = DeltaSet.t ;;

    let empty_typevar_set = GammaSet.empty ;;

    let add = DeltaSet.add ;;

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
        ok tau' (add x delta)
      | Typevar x -> DeltaSet.mem x delta ;;

    (* *************************************************************************
    ************************************************************************* *)


    (*
      subtype : Method to decide whether tau1 is a subtype of tau2 (currently
        just implemented as equality, will expand later).
    *)
    let subtype (tau1 : tp) (tau2 : tp) : bool =
      tau1 == tau2 ;;


    (*
      type_subst : Method to substitute a type variable according to
        tau1{tau2 / x}.
    *)
    let rec type_subst (tau1 : tp) (x : string) (tau2 : tp) : tp =
      match tau1 with
      | Unit
      | Int
      | Bool
      | String -> tau1
      | Record lst ->
        Record (List.map (fun (str, tau) -> (str, type_subst tau x tau2)) lst)
      | To (tau, tau') ->
        To (type_subst tau x tau2, type_subst tau' x tau2)
      | Sum (tau, tau') ->
        Sum (type_subst tau x tau2, type_subst tau' x tau2)
      | Forall (x', tau) ->
        if x' == x then tau1
        else Forall (x', type_subst tau x tau2)
      | Typevar x' ->
        if x' == x then tau2
        else tau1 ;;



    let new_type_var (delta : deltaset) : string =
      let str = "X" in
      let suff = ref 0 in
      let str' = ref str in
      let rec new_type_var' (delta' : deltaset) : string =
        if ok !str' delta'
          then suff := !suff + 1;
            str' := str ^ string_of_int !suff;
            new_type_var' delta' !str'
        else !str'
        in
      new_type_var' delta "X" ;;


    let rec type_check (gamma : gammaset) (delta : deltaset) (e : expr) : tp =
      match e with
      | U -> Unit
      | Var x ->
        (match type_of gamma x with
        | Some tau ->
          if ok tau delta then tau
          else raise (UnboundTypeVarException "Unbound type variable.")
        | None -> raise (UnboundVarException "Variable " ^ x ^ " unbound."))
      | Int _ -> Int
      | Neg e' ->
        if subtype (type_check gamma delta e') Int then Int
        else raise (TypeException "Type Int expected.")
      | Plus (e1, e2)
      | Times (e1, e2)
      | Pow (e1, e2) ->
        if subtype (type_check gamma delta e1) Int && subtype (type_check gamma delta e2) Int
          then Int
        else raise (TypeException "Type Int expected.")
      | Bool _ -> Bool
      | Equals (e1, e2)
      | Lessthan (e1, e2) ->
        if subtype (type_check gamma delta e1) Int && subtype (type_check gamma delta e2) Int
          then Bool
        else raise (TypeException "Type Int expected.")
      | Lam ((x, tau), e') ->
        let tau' = type_check (append gamma x tau) delta e' in
        if ok tau delta then To (tau, tau')
        else raise (UnboundTypeVarException "Unbound type variable.")
      | App (e1, e2) ->
        try
          let To (tau, tau') = type_check gamma delta e1 in
          let tau'' = type_check gamma delta e2 in
          if subtype tau'' tau then tau'
          else raise (ApplicationException "Type of argument does not match function signature.")
        with Match_failure ->
          raise (ApplicationException "Attempted to apply non-function.")
      | Let ((x, tau), e1, e2) ->
        if not (ok tau delta) then raise (UnboundTypeVarException "Unbound type variable.")
        else
          if subtype (type_check gamma delta e1) tau then
            type_check (append gamma x tau) delta e2
          else raise (TypeException "Type declaration does not match expression.")
      | Typelam (x, e') ->
        let tau = type_check gamma (add x delta) e' in
        Forall (x, tau)
      | Typeapp (e', tau) ->
        if not (ok tau delta) then raise (UnboundTypeVarException "Unbound type variable.")
        else
          try
            let Forall (x, tau') = type_check gamma delta e' in
            type_subst tau' x tau
          with Match_failure ->
            raise (TypeApplicationException "Type application failure.") ;;
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
    (* let x : tau = e1 in e2 *)
    | Let of (string * tp) * expr * expr
    | Typelam of string * expr
    | Typeapp of expr * tp =
  struct

  end

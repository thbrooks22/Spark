module Grammar =
  struct
    module type Type =
      sig
        type tp
        type expr

        type gammaset
        val empty_gamma : gammaset
        val type_of : gammaset -> string -> tp option
        val append : gammaset -> string -> tp -> gammaset

        type deltaset
        val empty_delta : deltaset
        val add : string -> deltaset -> deltaset
        val ok : tp -> deltaset -> bool

        val subtype : tp -> tp -> bool

        val type_subst : tp -> string -> tp -> tp

        val new_type_var : deltaset -> string -> string

        val type_check : gammaset -> deltaset -> expr -> tp

        val global_type_check : expr -> tp

        val string_of_tp : tp -> string
      end

      module type Expr =
        sig
          type tp
          type expr
          type evalcontext

          type varset
          val empty_set : varset
          val union : varset -> varset -> varset
          val append : string -> varset -> varset
          val remove : string -> varset -> varset
          val contains : string -> varset -> bool

          val free_vars : expr -> varset

          val new_var : varset -> string -> string

          val subst : expr -> string -> expr -> expr
        end
  end ;;

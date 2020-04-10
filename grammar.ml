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
        val add : 'a -> deltaset -> deltaset
        val ok : tp -> deltaset -> bool

        val subtype : tp -> tp -> bool

        val typse_subst : tp -> string -> tp -> tp

        val new_type_var : deltaset -> string

        val type_check : gammaset -> deltaset -> expr -> tp

        val global_type_check : expr -> tp
      end

      module type Expr =
        sig
          type tp
          type expr
        end
  end ;;

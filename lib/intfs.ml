type 'a outcome = Success of 'a | Failure

module Vocabulary = struct
  module type S = sig
    type input
    type token
    type spec
    type action = input list -> token option

    val vocabulary : (spec * action) list
  end
end

module Inputs = struct
  module type S = sig
    type t

    val compare : t -> t -> int
  end
end

module Tags = struct
  module type S = sig
    type t
    type output
    type args

    val compare : t -> t -> int
    val tag_to_action : t -> args -> output
  end
end

module StackSyms = struct
  module type S = sig
    type t

    val compare : t -> t -> int
  end
end

module Actions = struct
  module type S = sig
    type t

    val compare : t -> t -> int
  end
end

module Grammar = struct
  module type S = sig
    exception Fail

    type token
    type ast
    type terminal
    type nonterminal

    val string_of_terminal : terminal -> string
    val string_of_nonterminal : nonterminal -> string

    module Terminal : sig 
      type t = terminal 
      val equal : t -> t -> bool 
      val hash : t -> int 
      val compare : t -> t -> int 
    end 
    
    module Nonterminal : sig
      type t = nonterminal

      val equal : t -> t -> bool
      val hash : t -> int
      val compare : t -> t -> int
    end

    type data

    val unwrap : data -> ast

    type t = T of terminal | N of nonterminal
    type reduce = data list -> data
    type shift = token list -> data * token list

    type production = { rhs : t list; action : reduce }

    type rule =
      | Production of { lhs : nonterminal; rhss : production list }
      | Consumption of { lhs : terminal; action : shift }

    val grammar : rule list
  end
end

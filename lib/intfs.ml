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

    val compare : t -> t -> int
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

    module Terminal : sig
      type t [@@deriving compare, to_string]
    end

    type terminal = Terminal.t [@@deriving compare, to_string]

    module Nonterminal : sig
      type t [@@deriving compare, to_string]
    end

    type nonterminal = Nonterminal.t [@@deriving compare, to_string]

    val token_to_terminal : token -> terminal

    type data [@@deriving compare]

    val unwrap : data -> ast

    type t = T of terminal | N of nonterminal [@@deriving compare, to_string]
    type reduce = data list -> data
    type shift = token -> data
    type production = { rhs : t list; action : reduce }

    type rule =
      | Production of { lhs : nonterminal; rhss : production list }
      | Consumption of { lhs : terminal; action : shift }

    val grammar : rule list
    val start : nonterminal
  end
end

type 'a outcome = Success of 'a | Failure

module Inputs = struct 
  module type S = sig 
    type t 
    
    val compare : t -> t -> int 
  end 

end 

module Ast = struct 
  module type S = sig 
    type fparam
    type node
  end 
end

module Token = struct 
  module type S = sig
    type t
  end
end 

module Language = struct
  module type S = sig
    type token
    type ast
    type fparam
  end

  module Make (T : Token.S) (A : Ast.S) = struct
    type token = T.t
    type ast = A.node
    type fparam = A.fparam
  end
end

module Tags = struct
  module type S = sig
    type t
    type token
    type input

    val compare : t -> t -> int
    val tag_to_action : t -> input list -> token option
  end
end

module BNF = struct
  module type S = sig
    exception Fail

    type token
    type ast
    type terminal
    type nonterminal

    val string_of_terminal : terminal -> string
    val string_of_nonterminal : nonterminal -> string

    module Nonterminal : sig
      type t = nonterminal

      val equal : t -> t -> bool
      val hash : t -> int
      val compare : t -> t -> int
    end

    type data
    type parser = token list -> data * token list

    val unwrap : data -> ast

    type t = T of terminal | N of nonterminal
    type action = t list * (data list -> data)
    type actions = action list

    val terminal_to_parser : terminal -> parser
    val grammar : (nonterminal * actions) list
  end
end

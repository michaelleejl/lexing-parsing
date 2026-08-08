module type VOCABULARY = sig
  type input
  type token
  type spec
  type action = input list -> token option

  val rules : (spec * action) list
end

module type GRAMMAR = sig
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
  val eof : token

  type data [@@deriving compare]

  val unwrap : data -> ast

  type t = T of terminal | N of nonterminal [@@deriving compare, to_string]
  type reduce = data list -> data
  type shift = token -> data
  type production = { rhs : t list; action : reduce }

  type rule =
    | Production of { lhs : nonterminal; rhss : production list }
    | Consumption of { lhs : terminal; action : shift }

  val start : nonterminal
  val start_production : production
  val rules : rule list
end

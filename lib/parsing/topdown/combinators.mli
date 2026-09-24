open Lang

module Generalised (Grammar : GRAMMAR) : sig
  type token = Grammar.token
  type ast = Grammar.ast

  val parse : token list -> ast
end

module LL1 (Grammar : GRAMMAR) : sig
  type token = Grammar.token
  type ast = Grammar.ast

  val parse : token list -> ast
end

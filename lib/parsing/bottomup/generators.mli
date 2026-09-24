open Lang

module Generalised (Grammar : GRAMMAR) : sig
  type token = Grammar.token
  type ast = Grammar.ast

  val parse : token list -> ast
end

module SLR1 (Grammar : GRAMMAR) : sig
  type token = Grammar.token
  type ast = Grammar.ast

  val parse : token list -> ast
end

module LR1 (Grammar : GRAMMAR) : sig
  type token = Grammar.token
  type ast = Grammar.ast

  val parse : token list -> ast
end

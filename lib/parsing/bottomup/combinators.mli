open Lang

module General (Gram : GRAMMAR) : sig
  type token = Gram.token
  type ast = Gram.ast

  val parse : token list -> ast
end

module SLR1 (Gram : GRAMMAR) : sig
  type token = Gram.token
  type ast = Gram.ast

  val parse : token list -> ast
end


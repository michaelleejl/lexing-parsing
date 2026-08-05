open Intfs

module General (Gram : Grammar.S) : sig
  type token = Gram.token
  type ast = Gram.ast

  val parse : token list -> ast
end

module LL1 (Gram : Grammar.S) : sig 
  type token = Gram.token
  type ast = Gram.ast

  val parse : token list -> ast
end 

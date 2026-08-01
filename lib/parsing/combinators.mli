open Intfs

module Descent
    (Lang : Language.S)
    (Gram : Grammar.S with type token = Lang.token and type ast = Lang.ast) : sig
  type token = Lang.token
  type ast = Lang.ast

  val parse : token list -> ast
end

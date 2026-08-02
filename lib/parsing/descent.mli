open Intfs

module General : sig
  type token = Mlot.Token.t
  type ast = Mlot.Ast.node

  val parse : token list -> ast
end

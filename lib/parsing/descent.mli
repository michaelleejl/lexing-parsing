type token = Mlot.Token.t
type ast = Mlot.Ast.node

exception ParseFail of string

val parse : token list -> ast

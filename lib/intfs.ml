type 'a outcome = Success of 'a | Failure

module type Ast = sig 
  type fparam 
  type node 
end 

module type Token = sig 
  type t 
end 

module Language = struct 
  
  module type L = sig 
    type token 
    type ast 
    type fparam
  end 

  module Make (T : Token) (A : Ast) = struct
    type token = T.t
    type ast = A.node
    type fparam = A.fparam
  end 

end 

module type Language = (T: Token) -> (A : Ast) -> sig 
  type token = T.t 
  type ast = A.node 
  type fparam = A.fparam
end 
(* 
module type Lexer = (L: Language) -> sig
  exception LexFailure

  val lex : string -> L.Token.t list
end

module type Parser = (L: Language) -> sig
  exception ParseFailure

  val parse : L.Token.t list -> L.Ast.node
end *)

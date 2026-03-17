open Format

type token =
  | IDENT of string
  | NUM of int
  | TRUE
  | FALSE
  | FUN
  | ARROW
  | LPARAN
  | RPARAN
  | PLUS
  | LET
  | EQUALS
  | IN
  | REC

let token_to_string t =
  match t with
  | IDENT s -> sprintf "IDENT %s" s
  | NUM n -> sprintf "NUM %d" n
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
  | ARROW -> "ARROW"
  | LPARAN -> "LPARAN"
  | RPARAN -> "RPARAN"
  | PLUS -> "PLUS"
  | LET -> "LET"
  | EQUALS -> "EQUALS"
  | IN -> "IN"
  | REC -> "REC"

type fparam = FParam of string

and ast =
  | Var of string
  | Num of int
  | True
  | False
  | Fun of fparam * ast
  | App of ast * ast
  | Let of fparam * ast * ast
  | LetRec of fparam * ast * ast

module type Lexer = sig
  exception LexFailure

  val lex : string -> token list
end

module type Parser = sig
  exception ParseFailure

  val parse : token list -> ast
end

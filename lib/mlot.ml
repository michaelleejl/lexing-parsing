open Intfs
open Format

module Mlot_Token = struct
  type t =
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

  let to_str t =
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
end

module Mlot_Ast = struct
  type fparam = FParam of string

  and node =
    | Var of string
    | Num of int
    | True
    | False
    | Fun of fparam * node
    | App of node * node
    | Let of fparam * node * node
    | LetRec of fparam * node * node
end

module Mlot = Language.Make (Mlot_Token) (Mlot_Ast)

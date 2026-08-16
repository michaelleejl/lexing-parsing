open Ppx_compare_lib.Builtin
open Format

type t =
  | IDENT of string
  | NUM of int
  | TRUE
  | FALSE
  | FUN
  | ARROW
  | LPAREN
  | RPAREN
  | PLUS
  | LET
  | EQUALS
  | IN
  | REC
  | EOF
[@@deriving compare]

let to_str t =
  match t with
  | IDENT s -> sprintf "IDENT %s" s
  | NUM n -> sprintf "NUM %d" n
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
  | ARROW -> "ARROW"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | PLUS -> "PLUS"
  | LET -> "LET"
  | EQUALS -> "EQUALS"
  | IN -> "IN"
  | REC -> "REC"
  | EOF -> "EOF"

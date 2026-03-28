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
    | LPAREN
    | RPAREN
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
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
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
    | Bool of bool
    | Fun of fparam * node
    | App of node * node
    | Let of fparam * node * node
    | LetRec of fparam * node * node
    | Plus of node * node 
    | Equals of node * node 

  let fparam_to_str (FParam x) = x 

  let rec to_str t =
    match t with
    | Var x -> sprintf "%s" x
    | Num n -> sprintf "%d" n
    | Bool b -> sprintf "%b" b
    | Fun(x, e) -> sprintf "Fun(%s, %s)" (fparam_to_str x) (to_str e)
    | App(e1, e2) -> sprintf "App(%s, %s)" (to_str e1) (to_str e2)
    | Let(x, e1, e2) -> sprintf "Let(%s, %s, %s)" (fparam_to_str x) (to_str e1) (to_str e2)
    | LetRec(x, e1, e2) -> sprintf "LetRec(%s, %s, %s)" (fparam_to_str x) (to_str e1) (to_str e2)
    | Plus(e1, e2) -> sprintf "Plus(%s, %s)" (to_str e1) (to_str e2)
    | Equals(e1, e2) -> sprintf "Equals(%s, %s)" (to_str e1) (to_str e2)
end

module Mlot = Language.Make (Mlot_Token) (Mlot_Ast)

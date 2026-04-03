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
  type fparam = string

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

  let rec to_str t =
    match t with
    | Var x -> sprintf "%s" x
    | Num n -> sprintf "%d" n
    | Bool b -> sprintf "%b" b
    | Fun (x, e) -> sprintf "Fun(%s, %s)" x (to_str e)
    | App (e1, e2) -> sprintf "App(%s, %s)" (to_str e1) (to_str e2)
    | Let (x, e1, e2) -> sprintf "Let(%s, %s, %s)" x (to_str e1) (to_str e2)
    | LetRec (x, e1, e2) ->
        sprintf "LetRec(%s, %s, %s)" x (to_str e1) (to_str e2)
    | Plus (e1, e2) -> sprintf "Plus(%s, %s)" (to_str e1) (to_str e2)
    | Equals (e1, e2) -> sprintf "Equals(%s, %s)" (to_str e1) (to_str e2)
end

module Mlot = Language.Make (Mlot_Token) (Mlot_Ast)

module Tag = struct
  type t =
    | T_SKIP
    | T_IDENT
    | T_NUM
    | T_TRUE
    | T_FALSE
    | T_FUN
    | T_ARROW
    | T_LPAREN
    | T_RPAREN
    | T_PLUS
    | T_LET
    | T_EQUALS
    | T_IN
    | T_REC

  type input = char 
  
  type token = Mlot_Token.t

  let compare = compare

  let tag_to_action =
    let open Mlot_Token in
    function
    | T_TRUE -> fun _ -> Some TRUE
    | T_FALSE -> fun _ -> Some FALSE
    | T_FUN -> fun _ -> Some FUN
    | T_ARROW -> fun _ -> Some ARROW
    | T_LPAREN -> fun _ -> Some LPAREN
    | T_RPAREN -> fun _ -> Some RPAREN
    | T_PLUS -> fun _ -> Some PLUS
    | T_LET -> fun _ -> Some LET
    | T_EQUALS -> fun _ -> Some EQUALS
    | T_IN -> fun _ -> Some IN
    | T_REC -> fun _ -> Some REC
    | T_IDENT -> fun cs -> Some (IDENT (Base.String.of_list cs))
    | T_NUM ->
        fun cs -> Some (NUM (cs |> Base.String.of_list |> Base.Int.of_string))
    | T_SKIP -> fun _ -> None
end

module Mlot_Grammar = struct
  exception Fail

  type token = Mlot_Token.t
  type ast = Mlot_Ast.node

  type terminal =
    | IDENT
    | NUM
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
  [@@deriving to_string]

  type nonterminal = E | T' | T | F' | F | G' | G | S
  [@@deriving compare, equal, hash, to_string]

  module Nonterminal = struct
    type t = nonterminal [@@deriving compare, equal, hash]
  end

  type data =
    | None
    | Num of int
    | Name of string
    | Expr of ast
    | Exprs of ast list

  type parser = token list -> data * token list

  let unwrap = function Expr e -> e | _ -> failwith "Must be Expr type"

  type t = T of terminal | N of nonterminal
  type action = t list * (data list -> data)
  type actions = action list

  let terminal_to_parser t (toks : token list) =
    match (t, toks) with
    | IDENT, IDENT x :: toks' -> (Name x, toks')
    | NUM, NUM n :: toks' -> (Num n, toks')
    | TRUE, TRUE :: toks'
    | FALSE, FALSE :: toks'
    | FUN, FUN :: toks'
    | ARROW, ARROW :: toks'
    | LPAREN, LPAREN :: toks'
    | RPAREN, RPAREN :: toks'
    | PLUS, PLUS :: toks'
    | LET, LET :: toks'
    | EQUALS, EQUALS :: toks'
    | IN, IN :: toks'
    | REC, REC :: toks' ->
        (None, toks')
    | _ -> raise Fail

  open Mlot_Ast

  let mk_fun x e = Expr (Fun (x, e))
  let mk_let x e1 e2 = Expr (Let (x, e1, e2))
  let mk_letrec x e1 e2 = Expr (LetRec (x, e1, e2))
  let mk_var x = Expr (Var x)
  let mk_num n = Expr (Num n)
  let mk_bool b = Expr (Bool b)
  let eq e1 e2 = Equals (e1, e2)
  let pl e1 e2 = Plus (e1, e2)
  let ap e1 e2 = App (e1, e2)
  let mk_op op e es = Expr (List.fold_left op e es)
  let mk_eq e es = mk_op eq e es
  let mk_plus e es = mk_op pl e es
  let mk_app e es = mk_op ap e es
  let mk_nil = Exprs []
  let mk_cons e es = Exprs (e :: es)
  let mk_one e = Expr e

  (* e -> fun x -> e | let x = e in e *)
  let grammar : (nonterminal * actions) list =
    [
      ( E,
        [
          ( [ T FUN; T IDENT; T ARROW; N E ],
            [%act function [ _; Name x; _; Expr e ] -> mk_fun x e] );
          ( [ T LET; T IDENT; T EQUALS; N E; T IN; N E ],
            [%act
              function [ _; Name x; _; Expr e1; _; Expr e2 ] -> mk_let x e1 e2]
          );
          ( [ T LET; T REC; T IDENT; T EQUALS; N E; T IN; N E ],
            [%act
              function
              | [ _; _; Name x; _; Expr e1; _; Expr e2 ] -> mk_letrec x e1 e2]
          );
          ([ N T ], [%act function [ Expr e ] -> mk_one e]);
        ] );
      ( T,
        [
          ([ N F; N T' ], [%act function [ Expr e; Exprs es ] -> mk_eq e es]);
        ] );
      ( T',
        [
          ( [ T EQUALS; N F; N T' ],
            [%act function [ _; Expr e; Exprs es ] -> mk_cons e es] );
          ([], [%act function [] -> mk_nil]);
        ] );
      ( F,
        [
          ([ N G; N F' ], [%act function [ Expr e; Exprs es ] -> mk_plus e es]);
        ] );
      ( F',
        [
          ( [ T PLUS; N G; N F' ],
            [%act function [ _; Expr e; Exprs es ] -> mk_cons e es] );
          ([], [%act function [] -> mk_nil]);
        ] );
      ( G,
        [
          ([ N S; N G' ], [%act function [ Expr e; Exprs es ] -> mk_app e es]);
        ] );
      ( G',
        [
          ([ N S; N G' ], [%act function [ Expr e; Exprs es ] -> mk_cons e es]);
          ([], [%act function [] -> mk_nil]);
        ] );
      ( S,
        [
          ([ T IDENT ], [%act function [ Name x ] -> mk_var x]);
          ([ T NUM ], [%act function [ Num n ] -> mk_num n]);
          ([ T TRUE ], [%act function [ None ] -> mk_bool true]);
          ([ T FALSE ], [%act function [ None ] -> mk_bool false]);
          ( [ T LPAREN; N E; T RPAREN ],
            [%act function [ _; Expr e; _ ] -> mk_one e] );
        ] );
    ]
end

open Ppx_compare_lib.Builtin

module Common = struct
  exception Fail

  type token = Token.t [@@deriving compare]
  type ast = Ast.node [@@deriving compare]

  module Terminal = struct
    type t =
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
      | EOF
    [@@deriving compare, to_string]
  end

  type terminal = Terminal.t [@@deriving compare, to_string]

  type data =
    | None
    | Num of int
    | Name of string
    | Expr of ast
    | Exprs of ast list
  [@@deriving compare]

  let finish = function Expr e -> e | _ -> failwith "Must be Expr type"

  type builder = data list -> data
  type reader = token -> data

  let build b ds = b ds
  let read r tok = r tok

  let token_to_terminal (t : token) =
    match t with
    | IDENT _ -> Terminal.IDENT
    | NUM _ -> NUM
    | TRUE -> TRUE
    | FALSE -> FALSE
    | FUN -> FUN
    | ARROW -> ARROW
    | LPAREN -> LPAREN
    | RPAREN -> RPAREN
    | PLUS -> PLUS
    | LET -> LET
    | EQUALS -> EQUALS
    | IN -> IN
    | REC -> REC
    | EOF -> EOF

  (* appended by `parse`; no lexer rule ever produces it *)
  let eof = Token.EOF

  open Ast

  let mk_one e = Expr e
  let mk_nil = Exprs []
  let mk_cons e es = Exprs (e :: es)
  let mk_op op e es = Expr (List.fold_left op e es)
  let mk_fun x e = Expr (Fun (x, e))
  let mk_let x e1 e2 = Expr (Let (x, e1, e2))
  let mk_letrec x e1 e2 = Expr (LetRec (x, e1, e2))
  let mk_var x = Expr (Var x)
  let mk_num n = Expr (Num n)
  let mk_bool b = Expr (Bool b)
  let eq e1 e2 = Equals (e1, e2)
  let mk_eq e es = mk_op eq e es
  let pl e1 e2 = Plus (e1, e2)
  let mk_plus e es = mk_op pl e es
  let ap e1 e2 = App (e1, e2)
  let mk_app e es = mk_op ap e es
end

module General = struct
  include Common

  module Nonterminal = struct
    type t = E | T' | T | F' | F | G' | G | S [@@deriving compare, to_string]
  end

  type nonterminal = Nonterminal.t [@@deriving compare, to_string]

  module Sym = struct
    type t =
      | T of terminal [@stringable.nested ""]
      | N of nonterminal [@stringable.nested ""]
    [@@deriving compare, to_string]
  end

  type sym = Sym.t =
    | T of terminal [@stringable.nested ""]
    | N of nonterminal [@stringable.nested ""]
  [@@deriving compare, to_string]

  type production = { lhs : nonterminal; rhs : sym list; builder : builder }
  type consumption = { lhs : terminal; reader : reader }

  (* ---------- E ----------------*)
  let prod_e_fun =
    {
      lhs = E;
      rhs = [ T FUN; T IDENT; T ARROW; N E ];
      builder = [%act function [ _; Name x; _; Expr e ] -> mk_fun x e];
    }

  let prod_e_let =
    {
      lhs = E;
      rhs = [ T LET; T IDENT; T EQUALS; N E; T IN; N E ];
      builder =
        [%act
          function [ _; Name x; _; Expr e1; _; Expr e2 ] -> mk_let x e1 e2];
    }

  let prod_e_let_rec =
    {
      lhs = E;
      rhs = [ T LET; T REC; T IDENT; T EQUALS; N E; T IN; N E ];
      builder =
        [%act
          function
          | [ _; _; Name x; _; Expr e1; _; Expr e2 ] -> mk_letrec x e1 e2];
    }

  let prod_e_t =
    {
      lhs = E;
      rhs = [ N T ];
      builder = [%act function [ Expr e ] -> mk_one e];
    }

  let prod_e = [ prod_e_fun; prod_e_let; prod_e_let_rec; prod_e_t ]

  (* ---------- T ----------------*)
  (* T ::= F T' *)
  let prod_t_f =
    {
      lhs = Nonterminal.T;
      rhs = [ N F; N T' ];
      builder = [%act function [ Expr e; Exprs es ] -> mk_eq e es];
    }

  let prod_t = [ prod_t_f ]

  (* ---------- T' ---------------*)
  (* T' ::= = F T' | eps *)
  let prod_t'_equals =
    {
      lhs = T';
      rhs = [ T EQUALS; N F; N T' ];
      builder = [%act function [ _; Expr e; Exprs es ] -> mk_cons e es];
    }

  let prod_t'_eps =
    { lhs = T'; rhs = []; builder = [%act function [] -> mk_nil] }

  let prod_t' = [ prod_t'_equals; prod_t'_eps ]

  (* ---------- F ----------------*)
  (* F ::= G F' *)
  let prod_f_g =
    {
      lhs = F;
      rhs = [ N G; N F' ];
      builder = [%act function [ Expr e; Exprs es ] -> mk_plus e es];
    }

  let prod_f = [ prod_f_g ]

  (* ---------- F' ---------------*)
  (* F' ::= + G F' | eps *)
  let prod_f'_plus =
    {
      lhs = F';
      rhs = [ T PLUS; N G; N F' ];
      builder = [%act function [ _; Expr e; Exprs es ] -> mk_cons e es];
    }

  let prod_f'_eps =
    { lhs = F'; rhs = []; builder = [%act function [] -> mk_nil] }

  let prod_f' = [ prod_f'_plus; prod_f'_eps ]

  (* ---------- G ----------------*)
  (* G ::= S G' *)
  let prod_g_s =
    {
      lhs = G;
      rhs = [ N S; N G' ];
      builder = [%act function [ Expr e; Exprs es ] -> mk_app e es];
    }

  let prod_g = [ prod_g_s ]

  (* ---------- G' ---------------*)
  (* G' ::= S G' | eps *)
  let prod_g'_s =
    {
      lhs = G';
      rhs = [ N S; N G' ];
      builder = [%act function [ Expr e; Exprs es ] -> mk_cons e es];
    }

  let prod_g'_eps =
    { lhs = G'; rhs = []; builder = [%act function [] -> mk_nil] }

  let prod_g' = [ prod_g'_s; prod_g'_eps ]

  (* ---------- S ----------------*)
  (* S ::= x | n | true | false | ( E ) *)
  let prod_s_ident =
    {
      lhs = S;
      rhs = [ T IDENT ];
      builder = [%act function [ Name x ] -> mk_var x];
    }

  let prod_s_num =
    {
      lhs = S;
      rhs = [ T NUM ];
      builder = [%act function [ Num n ] -> mk_num n];
    }

  let prod_s_true =
    {
      lhs = S;
      rhs = [ T TRUE ];
      builder = [%act function [ None ] -> mk_bool true];
    }

  let prod_s_false =
    {
      lhs = S;
      rhs = [ T FALSE ];
      builder = [%act function [ None ] -> mk_bool false];
    }

  let prod_s_lparen =
    {
      lhs = S;
      rhs = [ T LPAREN; N E; T RPAREN ];
      builder = [%act function [ _; Expr e; _ ] -> mk_one e];
    }

  let prod_s =
    [ prod_s_ident; prod_s_num; prod_s_true; prod_s_false; prod_s_lparen ]

  (* ---------- terminals --------*)
  let cons_silent tok : consumption =
    let term = token_to_terminal tok in
    {
      lhs = term;
      reader = (function t when t = tok -> None | _ -> raise Fail);
    }

  let cons_ident : consumption =
    { lhs = IDENT; reader = [%act function Token.IDENT x -> Name x] }

  let cons_num : consumption =
    { lhs = NUM; reader = [%act function Token.NUM n -> Num n] }

  let cons_true = cons_silent TRUE
  let cons_false = cons_silent FALSE
  let cons_fun = cons_silent FUN
  let cons_arrow = cons_silent ARROW
  let cons_lparen = cons_silent LPAREN
  let cons_rparen = cons_silent RPAREN
  let cons_plus = cons_silent PLUS
  let cons_let = cons_silent LET
  let cons_equals = cons_silent EQUALS
  let cons_in = cons_silent IN
  let cons_rec = cons_silent REC
  let cons_eof = cons_silent EOF

  (* ---------- start ------------*)
  let start = Nonterminal.E

  let productions =
    List.concat
      [ prod_e; prod_t; prod_t'; prod_f; prod_f'; prod_g; prod_g'; prod_s ]

  let consumptions =
    [
      cons_ident;
      cons_num;
      cons_true;
      cons_false;
      cons_fun;
      cons_arrow;
      cons_lparen;
      cons_rparen;
      cons_plus;
      cons_let;
      cons_equals;
      cons_in;
      cons_rec;
      cons_eof;
    ]
end

module LeftFactored = struct
  include Common

  module Nonterminal = struct
    type t = E | E' | T' | T | F' | F | G' | G | S
    [@@deriving compare, to_string]
  end

  type nonterminal = Nonterminal.t [@@deriving compare, to_string]

  module Sym = struct
    type t =
      | T of terminal [@stringable.nested ""]
      | N of nonterminal [@stringable.nested ""]
    [@@deriving compare, to_string]
  end

  type sym = Sym.t =
    | T of terminal [@stringable.nested ""]
    | N of nonterminal [@stringable.nested ""]
  [@@deriving compare, to_string]

  type production = { lhs : nonterminal; rhs : sym list; builder : builder }
  type consumption = { lhs : terminal; reader : reader }

  (* ---------- E ----------------*)
  let prod_e_fun =
    {
      lhs = E;
      rhs = [ T FUN; T IDENT; T ARROW; N E ];
      builder = [%act function [ _; Name x; _; Expr e ] -> mk_fun x e];
    }

  let prod_e_let =
    {
      lhs = E;
      rhs = [ T LET; N E' ];
      builder = [%act function [ _; Expr e ] -> mk_one e];
    }

  let prod_e_t =
    {
      lhs = E;
      rhs = [ N T ];
      builder = [%act function [ Expr e ] -> mk_one e];
    }

  let prod_e = [ prod_e_fun; prod_e_let; prod_e_t ]

  (* ---------- E' ---------------*)
  let prod_e'_let =
    {
      lhs = E';
      rhs = [ T IDENT; T EQUALS; N E; T IN; N E ];
      builder =
        [%act function [ Name x; _; Expr e1; _; Expr e2 ] -> mk_let x e1 e2];
    }

  let prod_e'_let_rec =
    {
      lhs = E';
      rhs = [ T REC; T IDENT; T EQUALS; N E; T IN; N E ];
      builder =
        [%act
          function [ _; Name x; _; Expr e1; _; Expr e2 ] -> mk_letrec x e1 e2];
    }

  let prod_e' = [ prod_e'_let; prod_e'_let_rec ]

  (* ---------- T ----------------*)
  (* T ::= F T' *)
  let prod_t_f =
    {
      lhs = Nonterminal.T;
      rhs = [ N F; N T' ];
      builder = [%act function [ Expr e; Exprs es ] -> mk_eq e es];
    }

  let prod_t = [ prod_t_f ]

  (* ---------- T' ---------------*)
  (* T' ::= = F T' *)
  let prod_t'_equals =
    {
      lhs = T';
      rhs = [ T EQUALS; N F; N T' ];
      builder = [%act function [ _; Expr e; Exprs es ] -> mk_cons e es];
    }

  (*    | eps *)
  let prod_t'_eps =
    { lhs = T'; rhs = []; builder = [%act function [] -> mk_nil] }

  let prod_t' = [ prod_t'_equals; prod_t'_eps ]

  (* ---------- F ----------------*)
  (* F ::= G F' *)
  let prod_f_g =
    {
      lhs = F;
      rhs = [ N G; N F' ];
      builder = [%act function [ Expr e; Exprs es ] -> mk_plus e es];
    }

  let prod_f = [ prod_f_g ]

  (* ---------- F' ---------------*)
  (* F' ::= + G F' *)
  let prod_f'_plus =
    {
      lhs = F';
      rhs = [ T PLUS; N G; N F' ];
      builder = [%act function [ _; Expr e; Exprs es ] -> mk_cons e es];
    }

  (*    | eps *)
  let prod_f'_eps =
    { lhs = F'; rhs = []; builder = [%act function [] -> mk_nil] }

  let prod_f' = [ prod_f'_plus; prod_f'_eps ]

  (* ---------- G ----------------*)
  (* G ::= S G' *)
  let prod_g_s =
    {
      lhs = G;
      rhs = [ N S; N G' ];
      builder = [%act function [ Expr e; Exprs es ] -> mk_app e es];
    }

  let prod_g = [ prod_g_s ]

  (* ---------- G' ---------------*)
  (* G' ::= S G' *)
  let prod_g'_s =
    {
      lhs = G';
      rhs = [ N S; N G' ];
      builder = [%act function [ Expr e; Exprs es ] -> mk_cons e es];
    }

  (*    | eps *)
  let prod_g'_eps =
    { lhs = G'; rhs = []; builder = [%act function [] -> mk_nil] }

  let prod_g' = [ prod_g'_s; prod_g'_eps ]

  (* ---------- S ----------------*)
  (* S ::= x *)
  let prod_s_ident =
    {
      lhs = S;
      rhs = [ T IDENT ];
      builder = [%act function [ Name x ] -> mk_var x];
    }

  (*   | n *)

  let prod_s_num =
    {
      lhs = S;
      rhs = [ T NUM ];
      builder = [%act function [ Num n ] -> mk_num n];
    }

  (*   | true *)
  let prod_s_true =
    {
      lhs = S;
      rhs = [ T TRUE ];
      builder = [%act function [ None ] -> mk_bool true];
    }

  (*   | false *)
  let prod_s_false =
    {
      lhs = S;
      rhs = [ T FALSE ];
      builder = [%act function [ None ] -> mk_bool false];
    }

  (*   | ( E ) *)
  let prod_s_lparen =
    {
      lhs = S;
      rhs = [ T LPAREN; N E; T RPAREN ];
      builder = [%act function [ _; Expr e; _ ] -> mk_one e];
    }

  let prod_s =
    [ prod_s_ident; prod_s_num; prod_s_true; prod_s_false; prod_s_lparen ]

  (* ---------- terminals --------*)
  let cons_silent tok : consumption =
    let term = token_to_terminal tok in
    {
      lhs = term;
      reader = (function t when t = tok -> None | _ -> raise Fail);
    }

  let cons_ident : consumption =
    { lhs = IDENT; reader = [%act function Token.IDENT x -> Name x] }

  let cons_num : consumption =
    { lhs = NUM; reader = [%act function Token.NUM n -> Num n] }

  let cons_true = cons_silent TRUE
  let cons_false = cons_silent FALSE
  let cons_fun = cons_silent FUN
  let cons_arrow = cons_silent ARROW
  let cons_lparen = cons_silent LPAREN
  let cons_rparen = cons_silent RPAREN
  let cons_plus = cons_silent PLUS
  let cons_let = cons_silent LET
  let cons_equals = cons_silent EQUALS
  let cons_in = cons_silent IN
  let cons_rec = cons_silent REC
  let cons_eof = cons_silent EOF

  (* ---------- start ------------*)
  let start = Nonterminal.E

  let productions =
    List.concat
      [
        prod_e;
        prod_e';
        prod_t;
        prod_t';
        prod_f;
        prod_f';
        prod_g;
        prod_g';
        prod_s;
      ]

  let consumptions =
    [
      cons_ident;
      cons_num;
      cons_true;
      cons_false;
      cons_fun;
      cons_arrow;
      cons_lparen;
      cons_rparen;
      cons_plus;
      cons_let;
      cons_equals;
      cons_in;
      cons_rec;
      cons_eof;
    ]
end

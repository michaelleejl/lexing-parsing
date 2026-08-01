open Ppx_compare_lib.Builtin

exception Fail

type token = Token.t
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
  [@@deriving compare, to_string]
end

type terminal = Terminal.t [@@deriving compare, to_string]

module Nonterminal = struct
  type t = E | T' | T | F' | F | G' | G | S
  [@@deriving compare, to_string]
end

type nonterminal = Nonterminal.t [@@deriving compare, to_string]

type data =
  | None
  | Num of int
  | Name of string
  | Expr of ast
  | Exprs of ast list
   [@@deriving compare]

type parser = token list -> data * token list

let unwrap = function Expr e -> e | _ -> failwith "Must be Expr type"

type t = T of terminal [@stringable.nested ""]
       | N of nonterminal [@stringable.nested ""]
[@@deriving compare, to_string]

type reduce = data list -> data
type shift = token -> data

open Ast

  let token_to_terminal (t: token) = match t with
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

(* builders shared across several rules *)
let mk_one e = Expr e
let mk_nil = Exprs []
let mk_cons e es = Exprs (e :: es)
let mk_op op e es = Expr (List.fold_left op e es)

type production = {
  rhs: t list ;
  action : reduce
}

type rule = Production of {
  lhs: nonterminal;
  rhss: production list;
} | Consumption of {
  lhs: terminal;
  action: shift
}

(* ---------- E ----------------*)
(* E ::= fun x -> E*)
let mk_fun x e = Expr (Fun (x, e))
let prod_e_fun = {
  rhs = [ T FUN; T IDENT; T ARROW; N E ];
  action = [%act function [ _; Name x; _; Expr e ] -> mk_fun x e]
}

(*   | let x = E in E*)
let mk_let x e1 e2 = Expr (Let (x, e1, e2))
let prod_e_let = {
  rhs = [ T LET; T IDENT; T EQUALS; N E; T IN; N E ];
  action = [%act
    function [ _; Name x; _; Expr e1; _; Expr e2 ] -> mk_let x e1 e2
  ]
}

(*   | let rec x = E in E *)
let mk_letrec x e1 e2 = Expr (LetRec (x, e1, e2))
let prod_e_let_rec = {
  rhs = [ T LET; T REC; T IDENT; T EQUALS; N E; T IN; N E ];
  action =
    [%act
      function
      | [ _; _; Name x; _; Expr e1; _; Expr e2 ] -> mk_letrec x e1 e2]
}

(*   | T *)
let prod_e_t = {
  rhs = [ N T ];
  action = [%act function [ Expr e ] -> mk_one e]
}

let prod_e = Production
      { lhs = E;
        rhss =
          [
            prod_e_fun;
            prod_e_let;
            prod_e_let_rec;
            prod_e_t;
          ] }

(* ---------- T ----------------*)
(* T ::= F T' *)
let eq e1 e2 = Equals (e1, e2)
let mk_eq e es = mk_op eq e es

let prod_t_f =
  { rhs = [ N F; N T' ];
    action = [%act function [ Expr e; Exprs es ] -> mk_eq e es] }

let prod_t = Production { lhs = T; rhss = [ prod_t_f ] }

(* ---------- T' ---------------*)
(* T' ::= = F T' *)
let prod_t'_equals =
  { rhs = [ T EQUALS; N F; N T' ];
    action = [%act function [ _; Expr e; Exprs es ] -> mk_cons e es] }

(*    | eps *)
let prod_t'_eps = { rhs = []; action = [%act function [] -> mk_nil] }
let prod_t' = Production { lhs = T'; rhss = [ prod_t'_equals; prod_t'_eps ] }

(* ---------- F ----------------*)
(* F ::= G F' *)
let pl e1 e2 = Plus (e1, e2)
let mk_plus e es = mk_op pl e es

let prod_f_g =
  { rhs = [ N G; N F' ];
    action = [%act function [ Expr e; Exprs es ] -> mk_plus e es] }

let prod_f = Production { lhs = F; rhss = [ prod_f_g ] }

(* ---------- F' ---------------*)
(* F' ::= + G F' *)
let prod_f'_plus =
  { rhs = [ T PLUS; N G; N F' ];
    action = [%act function [ _; Expr e; Exprs es ] -> mk_cons e es] }

(*    | eps *)
let prod_f'_eps = { rhs = []; action = [%act function [] -> mk_nil] }
let prod_f' = Production { lhs = F'; rhss = [ prod_f'_plus; prod_f'_eps ] }

(* ---------- G ----------------*)
(* G ::= S G' *)
let ap e1 e2 = App (e1, e2)
let mk_app e es = mk_op ap e es

let prod_g_s =
  { rhs = [ N S; N G' ];
    action = [%act function [ Expr e; Exprs es ] -> mk_app e es] }

let prod_g = Production { lhs = G; rhss = [ prod_g_s ] }

(* ---------- G' ---------------*)
(* G' ::= S G' *)
let prod_g'_s =
  { rhs = [ N S; N G' ];
    action = [%act function [ Expr e; Exprs es ] -> mk_cons e es] }

(*    | eps *)
let prod_g'_eps = { rhs = []; action = [%act function [] -> mk_nil] }
let prod_g' = Production { lhs = G'; rhss = [ prod_g'_s; prod_g'_eps ] }

(* ---------- S ----------------*)
(* S ::= x *)
let mk_var x = Expr (Var x)

let prod_s_ident =
  { rhs = [ T IDENT ]; action = [%act function [ Name x ] -> mk_var x] }

(*   | n *)
let mk_num n = Expr (Num n)

let prod_s_num =
  { rhs = [ T NUM ]; action = [%act function [ Num n ] -> mk_num n] }

(*   | true | false *)
let mk_bool b = Expr (Bool b)

let prod_s_true =
  { rhs = [ T TRUE ]; action = [%act function [ None ] -> mk_bool true] }

let prod_s_false =
  { rhs = [ T FALSE ]; action = [%act function [ None ] -> mk_bool false] }

(*   | ( E ) *)
let prod_s_lparen =
  { rhs = [ T LPAREN; N E; T RPAREN ];
    action = [%act function [ _; Expr e; _ ] -> mk_one e] }

let prod_s =
  Production
    { lhs = S;
      rhss = [ prod_s_ident; prod_s_num; prod_s_true; prod_s_false; prod_s_lparen ]
    }

(* ---------- terminals --------*)
let cons_silent tok =
  let term = token_to_terminal tok in 
  Consumption
    { lhs = term;
      action =
        (function
        | t when t = tok -> None
        | _ -> raise Fail);
    }

let cons_ident =
  Consumption
    { lhs = IDENT; action = [%act function Token.IDENT x  -> Name x] }

let cons_num =
  Consumption
    { lhs = NUM; action = [%act function Token.NUM n -> Num n] }

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

let grammar : rule list =
  [
    prod_e;
    prod_t;
    prod_t';
    prod_f;
    prod_f';
    prod_g;
    prod_g';
    prod_s;
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
  ]

  let start = Nonterminal.E 

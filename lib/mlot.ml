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

module Mlot_Vocabulary = struct 

  open Regex
  open Mlot_Token 

    type input = char 
    type output = Mlot_Token.t option
    type spec = Regex.t 
    type action = input list -> output


  let vocabulary = [
    ((r "let"), (fun _ -> Some LET));
    ((r "rec"), (fun _ -> Some REC));
    ((r "in"), (fun _ -> Some IN));
    ((r "fun"), (fun _ -> Some FUN));
    ((r "true"), (fun _ -> Some TRUE));
    ((r "false"), (fun _ -> Some FALSE));
    ((r "="), (fun _ -> Some EQUALS));
    ((r {|\+|}), (fun _ -> Some PLUS));
    ((r "->"), (fun _ -> Some ARROW));
    ((r {|\(|}), (fun _ -> Some LPAREN));
    ((r {|\)|}), (fun _ -> Some RPAREN));
    ((r "[a-zA-Z][a-zA-Z0-9]*"), (fun cs -> Some (IDENT (Base.String.of_list cs))));
    ((r "-?[0-9]+"), (fun cs -> Some (NUM (cs |> Base.String.of_list |> Base.Int.of_string))));
    ((r {|\s|}), (fun _ -> None));
  ]

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
  [@@deriving compare, equal, hash, to_string]

  module Terminal = struct 
    type t = terminal [@@deriving compare, equal, hash]
  end 

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
  type reduce = data list -> data
  type shift = token list -> data * token list 
  open Mlot_Ast

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
  let cons_silent term tok =
    Consumption
      { lhs = term;
        action =
          (function
          | t :: toks' when t = tok -> (None, toks')
          | _ -> raise Fail);
      }
  let cons_ident =
    Consumption
      { lhs = IDENT; action = [%act function IDENT x :: toks' -> (Name x, toks')] }

  let cons_num =
    Consumption
      { lhs = NUM; action = [%act function NUM n :: toks' -> (Num n, toks')] }

  let cons_true = cons_silent TRUE TRUE 

  let cons_false = cons_silent FALSE FALSE

  let cons_fun = cons_silent FUN FUN

  let cons_arrow = cons_silent ARROW ARROW

  let cons_lparen = cons_silent LPAREN LPAREN 

  let cons_rparen = cons_silent RPAREN RPAREN

  let cons_plus = cons_silent PLUS PLUS

  let cons_let = cons_silent LET LET 

  let cons_equals = cons_silent EQUALS EQUALS

  let cons_in = cons_silent IN IN 

  let cons_rec = cons_silent REC REC

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
end

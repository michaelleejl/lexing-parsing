(*
     S ::= L = R | R
     L ::= * R | id
     R ::= L
*)

open Ppx_compare_lib.Builtin

exception Fail

type token = ID of string | EQ | STAR | END [@@deriving compare]
type ast = string [@@deriving compare]

module Terminal = struct
  type t = IDENT | EQUALS | ASTERISK | EOF [@@deriving compare, to_string]
end

type terminal = Terminal.t [@@deriving compare, to_string]

module Data = struct
  type t = Nothing | Name of string | Expr of string [@@deriving compare]
end

open Data

type data = Data.t [@@deriving compare]

let finish = function Expr e -> e | _ -> failwith "Must be Expr type"

type builder = data list -> data
type reader = token -> data

let build b ds = b ds
let read r tok = r tok

let token_to_terminal = function
  | ID _ -> Terminal.IDENT
  | EQ -> EQUALS
  | STAR -> ASTERISK
  | END -> EOF

let eof = END

module Nonterminal = struct
  type t = S | L | R [@@deriving compare, to_string]
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

(* ---------- S ----------------*)
(* S ::= L = R *)
let prod_s_assign =
  {
    lhs = Nonterminal.S;
    rhs = [ N L; T EQUALS; N R ];
    builder =
      (function
      | [ Expr l; _; Expr r ] -> Expr ("Assign(" ^ l ^ ", " ^ r ^ ")")
      | _ -> raise Fail);
  }

(*   | R *)
let prod_s_r =
  {
    lhs = Nonterminal.S;
    rhs = [ N R ];
    builder = (function [ Expr e ] -> Expr e | _ -> raise Fail);
  }

let prod_s = [ prod_s_assign; prod_s_r ]

(* ---------- L ----------------*)
(* L ::= * R *)
let prod_l_deref =
  {
    lhs = Nonterminal.L;
    rhs = [ T ASTERISK; N R ];
    builder =
      (function [ _; Expr r ] -> Expr ("Deref(" ^ r ^ ")") | _ -> raise Fail);
  }

(*   | id *)
let prod_l_ident =
  {
    lhs = Nonterminal.L;
    rhs = [ T IDENT ];
    builder = (function [ Name x ] -> Expr x | _ -> raise Fail);
  }

let prod_l = [ prod_l_deref; prod_l_ident ]

(* ---------- R ----------------*)
(* R ::= L *)
let prod_r_l =
  {
    lhs = Nonterminal.R;
    rhs = [ N L ];
    builder = (function [ Expr e ] -> Expr e | _ -> raise Fail);
  }

let prod_r = [ prod_r_l ]

(* ---------- terminals --------*)
let cons_ident =
  { lhs = Terminal.IDENT; reader = (function ID x -> Name x | _ -> raise Fail) }

let cons_silent lhs = { lhs; reader = (fun _ -> Nothing) }

(* ---------- start ------------*)
let start = Nonterminal.S
let productions = List.concat [ prod_s; prod_l; prod_r ]

let consumptions =
  [
    cons_ident;
    cons_silent Terminal.EQUALS;
    cons_silent Terminal.ASTERISK;
    cons_silent Terminal.EOF;
  ]

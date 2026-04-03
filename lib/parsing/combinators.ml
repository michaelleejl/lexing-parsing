open Mlot
open Mlot_Token
open Mlot_Ast

type token = Mlot_Token.t
type ast = Mlot_Ast.node

open Intfs

exception ParseFail of string

module Descent
    (Lang : Language.S)
    (Grammar : BNF.S with type token = Lang.token and type ast = Lang.ast) =
struct
  open Grammar

  type token = Lang.token
  type ast = Lang.ast

  let alt p1 p2 toks =
    match p1 toks with toks' -> toks' | exception ParseFail _ -> p2 toks

  let seq p1 p2 toks =
    let d1, toks' = p1 toks in
    let d2, toks'' = p2 toks' in
    (d1 @ d2, toks'')

  let ( >>| ) = alt
  let ( >>& ) = seq
  let empty ?(msg = "empty") _ = raise (ParseFail msg)
  let eps toks = ([], toks)

  let rec iota n =
    match n with
    | 0 -> []
    | n when n > 0 -> (n - 1) :: iota (n - 1)
    | _ -> failwith "n should be positive"

  let rec fix f x = f (fix f) x

  let fix_poly fs =
    fix (fun self fs -> List.map (fun f x -> f (self fs) x) fs) fs

  module NonterminalMap = Map.Make (Grammar.Nonterminal)

  let nonterminals : nonterminal list = List.map fst Grammar.grammar
  let productions : actions list = List.map snd Grammar.grammar

  let nt_to_idx =
    let _, map =
      List.fold_left
        (fun (idx, map) -> fun nt -> (idx + 1, NonterminalMap.add nt idx map))
        (0, NonterminalMap.empty) nonterminals
    in
    map

  let terminal_to_parser t toks =
    try Grammar.terminal_to_parser t toks
    with Grammar.Fail -> raise (ParseFail "terminal")

  let nonterminal_to_parser nt fs toks =
    (List.nth fs (NonterminalMap.find nt nt_to_idx)) toks

  let parser_to_matcher (x, y) = ([ x ], y)

  type matcher = token list -> data list * token list

  let pattern_to_matcher fs = function
    | T t -> Fun.compose parser_to_matcher (terminal_to_parser t)
    | N n -> Fun.compose parser_to_matcher (nonterminal_to_parser n fs)

  let production_to_matcher fs ps =
    List.fold_left ( >>& ) eps (List.map (pattern_to_matcher fs) ps)

  let production_to_parser fs (production, matcher_to_parser) toks =
    let matcher, toks' = production_to_matcher fs production toks in
    (matcher_to_parser matcher, toks')

  let productions_to_parsers fs (pss : Grammar.actions) =
    List.fold_left ( >>| ) (empty ~msg:"fail")
      (List.map (production_to_parser fs) pss)

  let parsers =
    List.map (fun p -> fun fs -> productions_to_parsers fs p) productions

  let parser = fix_poly parsers
  let start = List.nth parser 0

  let parse ts =
    match start ts with e, [] -> unwrap e | _ -> raise (ParseFail "fail")
end

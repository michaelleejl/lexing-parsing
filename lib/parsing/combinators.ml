open Intfs

exception ParseFail of string

module Descent (Gram : Grammar.S) = struct
  open Gram

  type token = Gram.token
  type ast = Gram.ast

  let alt p1 p2 toks = try p1 toks with ParseFail _ -> p2 toks

  let seq p1 p2 toks =
    let d1, toks' = p1 toks in
    let d2, toks'' = p2 toks' in
    (d1 @ d2, toks'')

  let ( >>| ) = alt
  let ( >>& ) = seq
  let empty _ = raise (ParseFail "empty")
  let eps toks = ([], toks)
  let rec fix f x = f (fix f) x

  let fix_poly fs =
    fix (fun self fs -> List.map (fun f x -> f (self fs) x) fs) fs

  module TerminalMap = Map.Make (Gram.Terminal)
  module NonterminalMap = Map.Make (Gram.Nonterminal)

  let production_rules, consumption_rules =
    List.partition_map
      (function
        | Production { lhs; rhss } -> Left (lhs, rhss)
        | Consumption { lhs; action } -> Right (lhs, action))
      Gram.grammar

  let (nonterminals, productions) : nonterminal list * production list list =
    List.split production_rules

  let nonterminal_map =
    let _, map =
      List.fold_left
        (fun (idx, map) -> fun nt -> (idx + 1, NonterminalMap.add nt idx map))
        (0, NonterminalMap.empty) nonterminals
    in
    map

  let terminal_map =
    List.fold_left
      (fun map -> function lhs, action -> TerminalMap.add lhs action map)
      TerminalMap.empty consumption_rules

  let terminal_to_parser t toks =
    let action = TerminalMap.find t terminal_map in
    try
      match toks with
      | tok :: toks' ->
          let t' = Gram.token_to_terminal tok in
          if t = t' then (action tok, toks')
          else raise (ParseFail "terminal mismatch")
      | [] -> raise (ParseFail "expected a terminal")
    with Fail -> raise (ParseFail "terminal")

  let nonterminal_to_parser nt fs toks =
    (List.nth fs (NonterminalMap.find nt nonterminal_map)) toks

  type accumulator = token list -> data list * token list

  let parser_to_accumulator (x, y) = ([ x ], y)

  let pattern_to_accumulator fs = function
    | T t -> Fun.compose parser_to_accumulator (terminal_to_parser t)
    | N n -> Fun.compose parser_to_accumulator (nonterminal_to_parser n fs)

  let production_to_accumulator fs ps =
    List.fold_left ( >>& ) eps (List.map (pattern_to_accumulator fs) ps)

  let production_to_parser fs { rhs; action } toks =
    let accumulator, toks' = production_to_accumulator fs rhs toks in
    (action accumulator, toks')

  let productions_to_parsers (pss : production list) fs =
    List.fold_left ( >>| ) empty (List.map (production_to_parser fs) pss)

  let parsers = List.map productions_to_parsers productions
  let parser = fix_poly parsers
  let start = List.nth parser (NonterminalMap.find Gram.start nonterminal_map)

  let parse ts =
    match start ts with e, [] -> unwrap e | _ -> raise (ParseFail "fail")
end

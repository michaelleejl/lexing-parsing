open Lang

exception ParseFail of string

module General (Grammar : GRAMMAR) = struct
  module Elaborated = Elaborate (Grammar)
  module Bnf = Elaborated.Bnf
  open Elaborated
  open Bnf

  type token = Elaborated.token
  type ast = Elaborated.ast

  let alt p1 p2 toks = try p1 toks with ParseFail _ -> p2 toks

  let seq p1 p2 toks =
    let d1, toks' = p1 toks in
    let d2, toks'' = p2 toks' in
    (d1 @ d2, toks'')

  let ( >>| ) = alt
  let ( >>& ) = seq
  let empty _ = raise (ParseFail "empty")
  let eps toks = ([], toks)

  module TerminalMap = Map.Make (Bnf.Terminal)
  module NonterminalMap = Map.Make (Bnf.Nonterminal)
  open Views (Bnf)

  let nonterminal_map =
    let _, map =
      List.fold_left
        (fun (idx, map) -> fun nt -> (idx + 1, NonterminalMap.add nt idx map))
        (0, NonterminalMap.empty) nonterminals
    in
    map

  let terminal_to_parser t toks =
    let action = consume t in
    try
      match toks with
      | tok :: toks' ->
          let t' = Grammar.token_to_terminal tok in
          if t = t' then (Grammar.shift action tok, toks')
          else raise (ParseFail "terminal mismatch")
      | [] -> raise (ParseFail "expected a terminal")
    with Fail -> raise (ParseFail "terminal")

  let nonterminal_to_parser nt fs toks =
    (List.nth fs (NonterminalMap.find nt nonterminal_map)) toks

  type accumulator = token list -> data list * token list

  let parser_to_accumulator (x, y) = ([ x ], y)

  let sym_to_accumulator fs = function
    | T t -> Fun.compose parser_to_accumulator (terminal_to_parser t)
    | N n -> Fun.compose parser_to_accumulator (nonterminal_to_parser n fs)

  let production_to_accumulator fs (p : production) =
    List.fold_left ( >>& ) eps (List.map (sym_to_accumulator fs) p.rhs)

  let production_to_parser fs p toks =
    let accumulator, toks' = production_to_accumulator fs p toks in
    (Grammar.reduce (action p) accumulator, toks')

  let productions_to_parsers (pss : production list) fs =
    List.fold_left ( >>| ) empty (List.map (production_to_parser fs) pss)

  let parsers =
    List.map productions_to_parsers
      (List.map productions_of_nonterminal nonterminals)

  let parser = Fixpoint.poly parsers
  let start_parser = List.nth parser (NonterminalMap.find start nonterminal_map)

  let parse ts =
    match start_parser (ts @ [ Grammar.eof ]) with
    | e, [] -> unwrap e
    | _ -> raise (ParseFail "fail")
end

module LL1 (Grammar : GRAMMAR) = struct
  module Elaborated = Elaborate (Grammar)
  open Elaborated
  module Bnf = Elaborated.Bnf
  open Bnf
  open Analysis

  type token = Grammar.token
  type ast = Grammar.ast

  open GrammarAnalysis (Bnf)

  type parser = token list -> data * token list
  type predictive_parser = { guard : TSet.t; parser : parser }

  let predict { guard = g1; parser = p1 } { guard = g2; parser = p2 } =
    if TSet.disjoint g1 g2 then
      let parser = function
        | tok :: _ as toks ->
            let term = Grammar.token_to_terminal tok in
            if TSet.mem term g1 then p1 toks
            else if TSet.mem term g2 then p2 toks
            else raise (ParseFail "unexpected token")
        | _ -> raise (ParseFail "bad parse")
      in
      { guard = TSet.union g1 g2; parser }
    else raise (ParseFail "grammar not in LL1")

  let seq p1 p2 toks =
    let d1, toks' = p1 toks in
    let d2, toks'' = p2 toks' in
    (d1 @ d2, toks'')

  let ( >>| ) = predict
  let ( >>& ) = seq
  let empty = { guard = TSet.empty; parser = (fun _ -> assert false) }
  let eps toks = ([], toks)

  module TerminalMap = Map.Make (Bnf.Terminal)
  module NonterminalMap = Map.Make (Bnf.Nonterminal)
  open Views (Bnf)

  let nonterminal_map =
    let _, map =
      List.fold_left
        (fun (idx, map) -> fun nt -> (idx + 1, NonterminalMap.add nt idx map))
        (0, NonterminalMap.empty) nonterminals
    in
    map

  let terminal_to_parser t toks =
    let action = consume t in
    try
      match toks with
      | tok :: toks' ->
          let t' = Grammar.token_to_terminal tok in
          if t = t' then (Grammar.shift action tok, toks')
          else raise (ParseFail "terminal mismatch")
      | [] -> raise (ParseFail "expected a terminal")
    with Fail -> raise (ParseFail "terminal")

  let nonterminal_to_parser nt fs toks =
    (List.nth fs (NonterminalMap.find nt nonterminal_map)) toks

  type accumulator = token list -> data list * token list

  let parser_to_accumulator (x, y) = ([ x ], y)

  let sym_to_accumulator fs = function
    | T t -> Fun.compose parser_to_accumulator (terminal_to_parser t)
    | N n -> Fun.compose parser_to_accumulator (nonterminal_to_parser n fs)

  let production_to_accumulator fs p =
    List.fold_left ( >>& ) eps (List.map (sym_to_accumulator fs) p.rhs)

  let production_to_parser fs lhs p =
    let parser toks =
      let accumulator, toks' = production_to_accumulator fs p toks in
      (Grammar.reduce (action p) accumulator, toks')
    in
    let guard =
      let first = First.syms p.rhs |> drop_eps in
      if Nullable.syms p.rhs then TSet.union first (Follow.nonterminal p.lhs)
      else first
    in
    { parser; guard }

  let productions_to_parsers ((lhs, pss) : nonterminal * production list) fs =
    let { parser } =
      List.fold_left ( >>| ) empty (List.map (production_to_parser fs lhs) pss)
    in
    parser

  let parsers = List.map productions_to_parsers production_rules
  let parser = Fixpoint.poly parsers
  let start_parser = List.nth parser (NonterminalMap.find start nonterminal_map)

  let parse ts =
    match start_parser (ts @ [ eof ]) with
    | e, [] -> unwrap e
    | _ -> raise (ParseFail "fail")
end

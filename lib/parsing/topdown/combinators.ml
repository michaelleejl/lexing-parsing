open Lang

exception Parse_error of string

module Generalised (Grammar : GRAMMAR) = struct
  module Augmented = Topdown_augment (Grammar)
  module Bnf = Augmented.Bnf
  open Augmented
  open Bnf

  type token = Augmented.token
  type ast = Augmented.ast

  let alt p1 p2 toks = try p1 toks with Parse_error _ -> p2 toks

  let seq p1 p2 toks =
    let d1, toks' = p1 toks in
    let d2, toks'' = p2 toks' in
    (d1 @ d2, toks'')

  let ( <|> ) = alt
  let ( <&> ) = seq
  let empty _ = raise (Parse_error "empty")
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
    let r = reader_of_terminal t in
    try
      match toks with
      | tok :: toks' ->
          let t' = token_to_terminal tok in
          if t = t' then (read r tok, toks')
          else raise (Parse_error "terminal mismatch")
      | [] -> raise (Parse_error "expected a terminal")
    with Fail -> raise (Parse_error "terminal")

  let nonterminal_to_parser nt fs toks =
    (List.nth fs (NonterminalMap.find nt nonterminal_map)) toks

  type accumulator = token list -> data list * token list

  let parser_to_accumulator (x, y) = ([ x ], y)

  let sym_to_accumulator fs = function
    | T t -> Fun.compose parser_to_accumulator (terminal_to_parser t)
    | N n -> Fun.compose parser_to_accumulator (nonterminal_to_parser n fs)

  let production_to_accumulator fs (p : production) =
    List.fold_left ( <&> ) eps (List.map (sym_to_accumulator fs) p.rhs)

  let production_to_parser fs p toks =
    let accumulator, toks' = production_to_accumulator fs p toks in
    (build (builder_of_production p) accumulator, toks')

  let productions_to_parsers (pss : production list) fs =
    List.fold_left ( <|> ) empty (List.map (production_to_parser fs) pss)

  let parsers =
    List.map productions_to_parsers
      (List.map productions_of_nonterminal nonterminals)

  let parser = Fixpoint.poly parsers
  let start_parser = List.nth parser (NonterminalMap.find start nonterminal_map)

  let parse ts =
    match start_parser (ts @ [ eof ]) with
    | e, [] -> finish e
    | _ -> raise (Parse_error "fail")
end

module LL1 (Grammar : GRAMMAR) = struct
  module Augmented = Topdown_augment (Grammar)
  open Augmented
  module Bnf = Augmented.Bnf
  open Bnf
  open Analysis

  type token = Grammar.token
  type ast = Grammar.ast

  open GrammarAnalysis (Bnf)

  type parser = token list -> data * token list
  type predictive_parser = { prediction : TSet.t; parser : parser }

  let predict { prediction = g1; parser = p1 } { prediction = g2; parser = p2 } =
    if TSet.disjoint g1 g2 then
      let parser = function
        | tok :: _ as toks ->
            let term = token_to_terminal tok in
            if TSet.mem term g1 then p1 toks
            else if TSet.mem term g2 then p2 toks
            else raise (Parse_error "unexpected token")
        | _ -> raise (Parse_error "bad parse")
      in
      { prediction = TSet.union g1 g2; parser }
    else raise (Parse_error "grammar not in LL1")

  let seq p1 p2 toks =
    let d1, toks' = p1 toks in
    let d2, toks'' = p2 toks' in
    (d1 @ d2, toks'')

  let ( <|> ) = predict
  let ( <&> ) = seq
  let empty = { prediction = TSet.empty; parser = (fun _ -> assert false) }
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
    let r = reader_of_terminal t in
    try
      match toks with
      | tok :: toks' ->
          let t' = token_to_terminal tok in
          if t = t' then (read r tok, toks')
          else raise (Parse_error "terminal mismatch")
      | [] -> raise (Parse_error "expected a terminal")
    with Fail -> raise (Parse_error "terminal")

  let nonterminal_to_parser nt fs toks =
    (List.nth fs (NonterminalMap.find nt nonterminal_map)) toks

  type accumulator = token list -> data list * token list

  let parser_to_accumulator (x, y) = ([ x ], y)

  let sym_to_accumulator fs = function
    | T t -> Fun.compose parser_to_accumulator (terminal_to_parser t)
    | N n -> Fun.compose parser_to_accumulator (nonterminal_to_parser n fs)

  let production_to_accumulator fs p =
    List.fold_left ( <&> ) eps (List.map (sym_to_accumulator fs) p.rhs)

  let production_to_parser fs lhs p =
    let parser toks =
      let accumulator, toks' = production_to_accumulator fs p toks in
      (build (builder_of_production p) accumulator, toks')
    in
    let prediction =
      let first = First.syms p.rhs |> to_terminals in
      if Nullable.syms p.rhs then TSet.union first (Follow.nonterminal p.lhs)
      else first
    in
    { parser; prediction }

  let productions_to_parsers ((lhs, pss) : nonterminal * production list) fs =
    let { parser } =
      List.fold_left ( <|> ) empty (List.map (production_to_parser fs lhs) pss)
    in
    parser

  let parsers = List.map productions_to_parsers production_rules
  let parser = Fixpoint.poly parsers
  let start_parser = List.nth parser (NonterminalMap.find start nonterminal_map)

  let parse ts =
    match start_parser (ts @ [ eof ]) with
    | e, [] -> finish e
    | _ -> raise (Parse_error "fail")
end

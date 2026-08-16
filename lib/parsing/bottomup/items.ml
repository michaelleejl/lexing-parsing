open Lang
open Ppx_compare_lib.Builtin

module type ITEM = sig
  include ELABORATED_BNF

  type t [@@deriving compare]
  type item = t
  type items = { start : item; accept : item; rest : item list }

  val is_complete : item -> bool
  val is_accept : item -> bool
  val items : items
  val next : item -> sym option
  val advance : item -> sym -> item option
  val eps : item -> item list
  val is_valid_for : item -> terminal -> bool
  val production_of : item -> production
end

module LR0 = struct
  module Make (Bnf : ELABORATED_BNF) :
    ITEM
      with module Terminal = Bnf.Terminal
       and module Nonterminal = Bnf.Nonterminal
       and module Sym = Bnf.Sym
       and type production = Bnf.production = struct
    include Bnf
    open Views (Bnf)
    open Analysis.GrammarAnalysis (Bnf)

    type t = { production : production; dot : int } [@@deriving compare]
    type item = t
    type items = { start : item; accept : item; rest : item list }

    let is_complete { production; dot } =
      let n = List.length production.rhs in
      dot = n

    let production_to_items ?(idx = 0) ?upto production acc =
      let upto = Option.value upto ~default:(List.length production.rhs) in
      let rec tr n acc =
        if n < idx then acc else tr (n - 1) ({ production; dot = n } :: acc)
      in
      tr upto acc

    let next item =
      let p = item.production in
      try Some (List.nth p.rhs item.dot) with Failure _ -> None

    let advance item sym =
      try
        let next = List.nth item.production.rhs item.dot in
        if sym = next then Some { item with dot = item.dot + 1 } else None
      with Failure _ -> None

    let zero production = { production; dot = 0 }

    let eps item =
      match next item with
      | Some (N n) -> List.map zero (productions_of_nonterminal n)
      | _ -> []

    let start_item = zero productions.start

    let accept_item =
      {
        production = productions.start;
        dot = List.length productions.start.rhs;
      }

    let is_accept item = item = accept_item

    let rest_items =
      production_to_items ~idx:1
        ~upto:(List.length productions.start.rhs - 1)
        productions.start
        (List.fold_right production_to_items productions.rest [])

    let items = { start = start_item; accept = accept_item; rest = rest_items }
    let production_of { production } = production

    let is_valid_for item terminal =
      let production = production_of item in
      let lhs = production.lhs in
      TSet.mem terminal (Follow.nonterminal lhs)
  end
end

module LR1 = struct
  module Make (Bnf : ELABORATED_BNF) :
    ITEM
      with module Terminal = Bnf.Terminal
       and module Nonterminal = Bnf.Nonterminal
       and module Sym = Bnf.Sym
       and type production = Bnf.production = struct
    include Bnf
    open Views (Bnf)
    open Analysis.GrammarAnalysis (Bnf)

    type t = { production : production; dot : int; lookahead : terminal }
    [@@deriving compare]

    type item = t
    type items = { start : item; accept : item; rest : item list }

    let is_complete { production; dot } =
      let n = List.length production.rhs in
      dot = n

    let production_to_items ?(idx = 0) ?upto production acc =
      let upto = Option.value upto ~default:(List.length production.rhs) in
      let rec tr n acc =
        let is =
          List.map
            (fun lookahead -> { production; dot = n; lookahead })
            terminals
        in
        if n < idx then acc else tr (n - 1) (is @ acc)
      in
      tr upto acc

    let next item =
      let p = item.production in
      try Some (List.nth p.rhs item.dot) with Failure _ -> None

    let advance item sym =
      try
        let next = List.nth item.production.rhs item.dot in
        if sym = next then Some { item with dot = item.dot + 1 } else None
      with Failure _ -> None

    let eps ({ production; dot; lookahead } as item) =
      match next item with
      | Some (N n) ->
          let suffix = List.drop (dot + 1) production.rhs in
          let productions = productions_of_nonterminal n in
          let lookaheads =
            First.syms (suffix @ [ T lookahead ]) |> drop_eps |> TSet.to_list
          in
          List.map
            (fun production ->
              List.map
                (fun lookahead -> { production; dot = 0; lookahead })
                lookaheads)
            productions
          |> List.flatten
      | _ -> []

    let start_item =
      { production = productions.start; dot = 0; lookahead = eof_terminal }

    let accept_item =
      {
        production = productions.start;
        dot = List.length productions.start.rhs;
        lookahead = eof_terminal;
      }

    let is_accept item = item = accept_item

    let rest_items =
      production_to_items ~idx:1
        ~upto:(List.length productions.start.rhs - 1)
        productions.start
        (List.fold_right production_to_items productions.rest [])

    let items = { start = start_item; accept = accept_item; rest = rest_items }
    let production_of { production } = production
    let is_valid_for item terminal = item.lookahead = terminal
  end
end

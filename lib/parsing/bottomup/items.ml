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
  end
end

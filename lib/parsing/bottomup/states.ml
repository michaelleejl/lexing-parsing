open Lang
open Items
open Automata

module type STATE = sig
  module Item : ITEM
  module ItemSet : Set.S with type elt = Item.t
  module Dfa : Dfa.S with type input = Item.Sym.t
  module Nfa : Nfa.S with type input = Item.Sym.t

  type t = Dfa.state [@@deriving compare]

  val initial : t
  val all_states : t list
  val next : t -> Item.Sym.t -> t
  val is_accepting : t -> bool
  val items_of : t -> ItemSet.t
end

module Make (Item : ITEM) : STATE with module Item = Item = struct
  module Item = Item
  module ItemSet = Set.Make (Item)
  open Item
  open Views (Item)
  module ItemMap = Map.Make (Item)
  module IntMap = Map.Make (Int)
  module Dfa = Dfa.Make (Item.Sym)
  module Nfa = Dfa.Nfa

  type t = Dfa.state [@@deriving compare]

  let _, (states, item_to_state_map, state_to_item_map) =
    List.fold_left
      (fun (n, (states, encoder, decoder)) it ->
        ( n + 1,
          ( Nfa.StateSet.add n states,
            ItemMap.add it n encoder,
            IntMap.add n it decoder ) ))
      (0, (Nfa.StateSet.empty, ItemMap.empty, IntMap.empty))
      (items.start :: items.accept :: items.rest)

  let item_to_state x = ItemMap.find x item_to_state_map
  let state_to_item x = IntMap.find x state_to_item_map
  let initial = item_to_state items.start
  let finals = Nfa.StateSet.singleton (item_to_state items.accept)

  let next state =
    let item = state_to_item state in
    match next item with
    | None -> Nfa.InputOptMap.empty
    | Some (T t) as sym ->
        Nfa.InputOptMap.singleton sym
          (Nfa.StateSet.singleton
             (item_to_state (Option.get (advance item (T t)))))
    | Some (N n) as sym ->
        Nfa.InputOptMap.of_list
          [
            ( sym,
              Nfa.StateSet.singleton
                (item_to_state (Option.get (advance item (N n)))) );
            (None, Nfa.StateSet.of_list @@ List.map item_to_state (eps item));
          ]

  let alphabet = Nfa.InputSet.of_list symbols
  let nfa = Nfa.{ states; initial; finals; next; alphabet }
  let Dfa.{ dfa; subsets } = Dfa.subset_construction nfa
  let initial = Dfa.initialise dfa
  let all_states = Dfa.StateSet.elements Dfa.(dfa.states)
  let next state input = Dfa.step dfa state input
  let is_accepting state = Dfa.is_accepting dfa state

  let items_of state =
    Nfa.StateSet.fold
      (fun state acc -> ItemSet.add (state_to_item state) acc)
      (subsets state) ItemSet.empty
end

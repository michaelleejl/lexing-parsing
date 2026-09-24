open Lang
open Items
open Automata

module type STATE = sig
  module Item : ITEM
  module Item_set : Set.S with type elt = Item.t
  module Dfa : Dfa.S with type input = Item.Sym.t
  module Nfa : Nfa.S with type input = Item.Sym.t

  type t = Dfa.state [@@deriving compare]

  val initial : t
  val all_states : t list
  val next : t -> Item.Sym.t -> t
  val is_accepting : t -> bool
  val items_of : t -> Item_set.t
end

module Make (Item : ITEM) : STATE with module Item = Item = struct
  module Item = Item
  module Item_set = Set.Make (Item)
  open Item
  open Views (Item)
  module Item_map = Map.Make (Item)
  module Int_map = Map.Make (Int)
  module Dfa = Dfa.Make (Item.Sym)
  module Nfa = Dfa.Nfa

  type t = Dfa.state [@@deriving compare]

  let _, (states, item_to_state_map, state_to_item_map) =
    List.fold_left
      (fun (n, (states, encoder, decoder)) it ->
        ( n + 1,
          ( Nfa.State_set.add n states,
            Item_map.add it n encoder,
            Int_map.add n it decoder ) ))
      (0, (Nfa.State_set.empty, Item_map.empty, Int_map.empty))
      (items.start :: items.accept :: items.rest)

  let item_to_state x = Item_map.find x item_to_state_map
  let state_to_item x = Int_map.find x state_to_item_map
  let initial = item_to_state items.start
  let finals = Nfa.State_set.singleton (item_to_state items.accept)

  let next state =
    let item = state_to_item state in
    match next item with
    | None -> Nfa.Input_opt_map.empty
    | Some (T t) as sym ->
        Nfa.Input_opt_map.singleton sym
          (Nfa.State_set.singleton
             (item_to_state (Option.get (advance item (T t)))))
    | Some (N n) as sym ->
        Nfa.Input_opt_map.of_list
          [
            ( sym,
              Nfa.State_set.singleton
                (item_to_state (Option.get (advance item (N n)))) );
            (None, Nfa.State_set.of_list @@ List.map item_to_state (eps item));
          ]

  let alphabet = Nfa.Input_set.of_list symbols
  let nfa = Nfa.{ states; initial; finals; next; alphabet }
  let Dfa.{ dfa; subsets } = Dfa.subset_construction nfa
  let initial = Dfa.initialise dfa
  let all_states = Dfa.State_set.elements Dfa.(dfa.states)
  let next state input = Dfa.step dfa state input
  let is_accepting state = Dfa.is_accepting dfa state

  let items_of state =
    Nfa.State_set.fold
      (fun state acc -> Item_set.add (state_to_item state) acc)
      (subsets state) Item_set.empty
end

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

module Make (Item : ITEM) : STATE with module Item = Item

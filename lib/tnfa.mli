open Intfs

module type S = sig
  type state = Nfa.state

  module StateSet : Set.S with type elt = state
  module CharSet : Set.S with type elt = char
  module CharOptMap : Map.S with type key = char option
  module StateMap : Map.S with type key = state

  type tag
  type transition = StateSet.t CharOptMap.t
  type state_set = StateSet.t
  type char_set = CharSet.t
  type tag_lookup = tag StateMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : char_set;
    tagger : tag_lookup;
  }

  val lift : Nfa.t -> tag -> t
  val alt : t -> t -> t
  val initialise : t -> state_set
  val is_rejecting : t -> state_set -> bool
  val is_accepting : t -> state_set -> bool
  val step : t -> state_set -> char -> state_set
  val emit_tag : t -> state_set -> tag option
end

module Make (Tag : Tags.S) : S with type tag = Tag.t

open Intfs

module type S = sig
  type tag

  module TaggedNfa : Tnfa.S with type tag = tag
  module StateSet : Set.S with type elt = TaggedNfa.StateSet.elt
  module StateMap : Map.S with type key = int
  module CharSet : Set.S with type elt = TaggedNfa.CharSet.elt
  module CharMap : Map.S with type key = char

  type state = StateSet.elt
  type state_set = StateSet.t
  type char_set = CharSet.t
  type transition = state CharMap.t
  type tag_lookup = tag option StateMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : char_set;
    tagger : tag_lookup;
  }

  val determinise : TaggedNfa.t -> t
  val initialise : t -> state
  val is_rejecting : t -> state -> bool
  val is_accepting : t -> state -> bool
  val step : t -> state -> char -> state
  val emit_tag : t -> state -> tag option
end

module Make (Tag : Tags.S) : S with type tag = Tag.t

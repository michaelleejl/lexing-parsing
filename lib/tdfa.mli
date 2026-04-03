open Intfs

module type S = sig
  type input 
  type tag

  module TaggedNfa : Tnfa.S with type tag = tag and type input = input
  module StateSet : Set.S with type elt = TaggedNfa.StateSet.elt
  module StateMap : Map.S with type key = int
  module InputSet : Set.S with type elt = TaggedNfa.InputSet.elt
  module InputMap : Map.S with type key = input

  type state = StateSet.elt
  type state_set = StateSet.t
  type input_set = InputSet.t
  type transition = state InputMap.t
  type tag_lookup = tag option StateMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : input_set;
    tagger : tag_lookup;
  }

  val determinise : TaggedNfa.t -> t
  val initialise : t -> state
  val is_rejecting : t -> state -> bool
  val is_accepting : t -> state -> bool
  val step : t -> state -> input -> state
  val emit_tag : t -> state -> tag option
end

module Make (Input: Inputs.S) (Tag : Tags.S with type input = Input.t) : S with type tag = Tag.t and type input = Input.t 

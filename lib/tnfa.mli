open Intfs

module type S = sig
  type input 

  module Nfa: Nfa.S with type input = input 

  type state = Nfa.state

  module StateSet : Set.S with type elt = state
  module InputSet : Set.S with type elt = input
  module InputOptMap : Map.S with type key = input option
  module StateMap : Map.S with type key = state

  type tag
  type transition = StateSet.t InputOptMap.t
  type state_set = StateSet.t
  type input_set = InputSet.t
  type tag_lookup = tag StateMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
    tagger : tag_lookup;
  }

  val lift : Nfa.t -> tag -> t
  val alt : t -> t -> t
  val initialise : t -> state_set
  val is_rejecting : t -> state_set -> bool
  val is_accepting : t -> state_set -> bool
  val step : t -> state_set -> input -> state_set
  val emit_tag : t -> state_set -> tag option
end

module Make (Input: Inputs.S) (Tag : Tags.S with type input = Input.t) : S with type tag = Tag.t and type input = Input.t 

open Params

module type S = sig
  type input

  module Nfa : Nfa.S with type input = input

  type state = Nfa.state

  module State_set : Set.S with type elt = state
  module Input_set : Set.S with type elt = input
  module Input_opt_map : Map.S with type key = input option
  module State_map : Map.S with type key = state

  type tag
  type transition = State_set.t Input_opt_map.t
  type state_set = State_set.t
  type input_set = Input_set.t
  type tag_lookup = tag State_map.t

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

module Make (Input : INPUT) (Tag : TAG) :
  S with type tag = Tag.t and type input = Input.t

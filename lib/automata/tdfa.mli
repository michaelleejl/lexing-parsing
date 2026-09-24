open Params

module type S = sig
  type input
  type tag

  module Tagged_nfa : Tnfa.S with type tag = tag and type input = input
  module State_set : Set.S with type elt = Tagged_nfa.State_set.elt
  module State_map : Map.S with type key = int
  module Input_set : Set.S with type elt = Tagged_nfa.Input_set.elt
  module Input_map : Map.S with type key = input

  type state = State_set.elt
  type state_set = State_set.t
  type input_set = Input_set.t
  type transition = state Input_map.t
  type tag_lookup = tag option State_map.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : input_set;
    tagger : tag_lookup;
  }

  type determinisation = { dfa : t; subsets : state -> Tagged_nfa.state_set }

  val subset_construction : Tagged_nfa.t -> determinisation
  val determinise : Tagged_nfa.t -> t
  val initialise : t -> state
  val is_rejecting : t -> state -> bool
  val is_accepting : t -> state -> bool
  val step : t -> state -> input -> state
  val emit_tag : t -> state -> tag option
end

module Make (Input : INPUT) (Tag : TAG) :
  S with type tag = Tag.t and type input = Input.t

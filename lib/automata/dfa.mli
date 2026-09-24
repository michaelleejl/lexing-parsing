open Params

module type S = sig
  type input

  module Nfa : Nfa.S with type input = input
  module State_set : Set.S with type elt = Nfa.State_set.elt
  module State_map : Map.S with type key = int
  module Input_set : Set.S with type elt = Nfa.Input_set.elt
  module Input_map : Map.S with type key = input

  type state = State_set.elt [@@deriving compare]
  type state_set = State_set.t
  type input_set = Input_set.t
  type transition = state Input_map.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : input_set;
  }

  type determinisation = { dfa : t; subsets : state -> Nfa.state_set }

  val subset_construction : Nfa.t -> determinisation
  val determinise : Nfa.t -> t
  val initialise : t -> state
  val is_rejecting : t -> state -> bool
  val is_accepting : t -> state -> bool
  val step : t -> state -> input -> state
  val accept : t -> input list -> bool
end

module Make (Input : INPUT) : S with type input = Input.t

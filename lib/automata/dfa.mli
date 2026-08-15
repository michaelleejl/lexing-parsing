open Params

module type S = sig
  type input

  module Nfa : Nfa.S with type input = input
  module StateSet : Set.S with type elt = Nfa.StateSet.elt
  module StateMap : Map.S with type key = int
  module InputSet : Set.S with type elt = Nfa.InputSet.elt
  module InputMap : Map.S with type key = input

  type state = StateSet.elt
  type state_set = StateSet.t
  type input_set = InputSet.t
  type transition = state InputMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : input_set;
  }

  type determinisation = {
    dfa : t;
    subsets : state -> Nfa.state_set;
  }

  val subset_construction: Nfa.t -> determinisation
  val determinise : Nfa.t -> t
  val initialise : t -> state
  val is_rejecting : t -> state -> bool
  val is_accepting : t -> state -> bool
  val step : t -> state -> input -> state
  val accept : t -> input list -> bool
end

module Make (Input : INPUT) : S with type input = Input.t

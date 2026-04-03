open Intfs 

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
    next : state -> transition;
    alphabet : input_set;
  }

  val determinise : Nfa.t -> t
  val accept : t -> input list -> bool

end

module Make (Input: Inputs.S): S with type input = Input.t 
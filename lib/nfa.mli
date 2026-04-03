open Intfs 

module type S = sig 
  type state = int
  type input

  module State = Int
  module StateSet : Set.S with type elt = state
  module InputSet : Set.S with type elt = input
  module InputOptMap : Map.S with type key = input option

  type transition = StateSet.t InputOptMap.t
  type state_set = StateSet.t
  type input_set = InputSet.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
  }

  val empty : t
  val epsilon : t
  val one_of : input list -> t
  val alt : t -> t -> t
  val seq : t -> t -> t
  val kleene : t -> t
  val accept : t -> input list -> bool
  val initialise : t -> state_set
  val is_accepting : t -> state_set -> bool
  val is_rejecting : t -> state_set -> bool
  val step : t -> state_set -> input -> state_set
end 

module Make (Input: Inputs.S) : S with type input = Input.t 

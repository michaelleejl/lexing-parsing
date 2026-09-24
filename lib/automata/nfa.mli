open Params

module type S = sig
  type state = int
  type input

  module State = Int
  module State_set : Set.S with type elt = state
  module Input_set : Set.S with type elt = input
  module Input_opt_map : Map.S with type key = input option

  type transition = State_set.t Input_opt_map.t
  type state_set = State_set.t
  type input_set = Input_set.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
  }

  val init :
    state_set -> state -> state_set -> (state -> transition) -> input_set -> t

  val empty : t
  val epsilon : t
  val one_of : ?alphabet:input_set -> input list -> t
  val alt : t -> t -> t
  val seq : t -> t -> t
  val kleene : t -> t
  val accept : t -> input list -> bool
  val initialise : t -> state_set
  val is_accepting : t -> state_set -> bool
  val is_rejecting : t -> state_set -> bool
  val step : t -> state_set -> input -> state_set
end

module Make (Input : INPUT) : S with type input = Input.t

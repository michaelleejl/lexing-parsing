type state = int

module StateSet : Set.S with type elt = state
module CharSet : Set.S with type elt = char
module CharOptMap : Map.S with type key = char option

type transition = StateSet.t CharOptMap.t
type state_set = StateSet.t
type char_set = CharSet.t

type t = {
  initial : state;
  finals : state_set;
  next : state -> transition;
  alphabet : char_set;
}

val empty : t
val epsilon : t
val one_of : char list -> t
val alt : t -> t -> t
val seq : t -> t -> t
val kleene : t -> t
val accept : t -> string -> bool
val initialise : t -> state_set
val is_final : t -> state -> bool
val contains_final : t -> state_set -> bool
val step : t -> state_set -> char -> state_set

type state = int 
module StateSet: Set.S with type elt = state 
module CharOptMap : Map.S with type key = char option
type transition = StateSet.t CharOptMap.t

type nfa  = { initial : state; finals : StateSet.t; next : state -> transition }

val empty : nfa
val epsilon : nfa
val one_of : char list -> nfa
val alt : nfa -> nfa -> nfa
val seq : nfa -> nfa -> nfa
val kleene : nfa -> nfa
val accept : nfa -> string -> bool

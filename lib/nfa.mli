type state = int 
module StateSet: Set.S with type elt = state 
module CharSet : Set.S with type elt = char 

module CharOptMap : Map.S with type key = char option

type transition = StateSet.t CharOptMap.t
type state_set = StateSet.t 
type char_set = CharSet.t 
type nfa  = { initial : state; finals : state_set; next : state -> transition; alphabet: char_set }

val empty : nfa
val epsilon : nfa
val one_of : char list -> nfa
val alt : nfa -> nfa -> nfa
val seq : nfa -> nfa -> nfa
val kleene : nfa -> nfa
val accept : nfa -> string -> bool

val initialise : nfa -> state_set
val step: nfa -> state_set -> char -> state_set
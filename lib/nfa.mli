type nfa

val empty : nfa
val epsilon : nfa
val one_of : char list -> nfa
val alt : nfa -> nfa -> nfa
val seq : nfa -> nfa -> nfa
val kleene : nfa -> nfa
val accept : nfa -> string -> bool

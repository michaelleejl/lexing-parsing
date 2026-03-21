type t

val empty : t
val epsilon : t
val chr : char -> t
val str : string -> t
val alt : t -> t -> t
val seq : t -> t -> t
val kleene : t -> t
val plus : t -> t
val opt : t -> t
val compile : t -> Nfa.t
val parse : string -> t

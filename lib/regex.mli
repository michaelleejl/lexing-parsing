open Nfa 

type t 

val empty : t 
val epsilon : t 
val chr : char -> t 
val alt : t -> t -> t 
val seq : t -> t -> t 
val kleene : t -> t
val plus : t -> t 
val opt : t -> t

val compile : t -> nfa 
val parse : string -> t 
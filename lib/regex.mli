module C : Set.S with type elt = char

type 'c rgx =
  | Empty
  | Epsilon
  | Char of 'c
  | Alt of 'c rgx * 'c rgx
  | Seq of 'c rgx * 'c rgx
  | Kleene of 'c rgx

type t = C.t rgx

val empty : t
val epsilon : t
val chr : char -> t
val str : string -> t
val ( >| ) : t -> t -> t
val ( >& ) : t -> t -> t
val ( ~* ) : t -> t
val ( ~+ ) : t -> t
val ( ~? ) : t -> t
val r : string -> t

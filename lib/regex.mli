module Charset : Set.S with type elt = char

type 'c regex =
  | Empty
  | Epsilon
  | Chars of 'c
  | Alt of 'c regex * 'c regex
  | Seq of 'c regex * 'c regex
  | Kleene of 'c regex

type t = Charset.t regex

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

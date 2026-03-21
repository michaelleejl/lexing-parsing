module Recogniser : sig
  type t
  type s

  val empty : t
  val epsilon : t
  val chr : char -> t
  val str : string -> t
  val ( >| ) : t -> t -> t
  val ( >& ) : t -> t -> t
  val ( ~* ) : t -> t
  val ( ~+ ) : t -> t
  val ( ~? ) : t -> t
  val parse : string -> t
  val generate : t -> s
  val recognise : s -> string -> bool
end

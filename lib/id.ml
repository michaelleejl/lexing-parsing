module type ID = sig
  type t

  val compare : t -> t -> int
end

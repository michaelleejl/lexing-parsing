module type INPUT = sig
  type t

  val compare : t -> t -> int
end

module type STACK_SYM = sig
  type t

  val compare : t -> t -> int
end

module type TAG = sig
  type t

  val compare : t -> t -> int
end

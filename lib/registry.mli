module type ELT = sig
  type elt
end

module type S = sig
  module Id : Id.ID

  type elt

  val register : elt -> Id.t
  val get : Id.t -> elt
end

module Make (Elt : ELT) : S with type elt = Elt.elt

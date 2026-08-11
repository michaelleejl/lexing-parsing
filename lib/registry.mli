module type ELT = sig
  type elt
end

module type S = sig
  module Tag : Automata.Params.TAG
  
  type elt

  val register : elt -> Tag.t

  val get : Tag.t -> elt

end

module Make (Action : ELT) : S with type elt = Action.elt

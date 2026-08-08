open Intfs

module type ACTION = sig
  type t
end

module type S = sig
  module Tag : Tags.S
  
  type action

  val register : action -> Tag.t

  val get : Tag.t -> action

end

module Make (Action : ACTION) : S with type action = Action.t

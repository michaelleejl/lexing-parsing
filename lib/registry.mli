open Intfs

module type ACTION = sig
  type t
end

module type S = sig
  module Tag : Tags.S
  
  type action
  type table

  val empty : table

  val register : action -> table -> Tag.t * table

  val get : Tag.t -> table -> action

end

module Make (Action : ACTION) : S with type action = Action.t

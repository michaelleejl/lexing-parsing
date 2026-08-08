module type ACTION = sig
  type t
end

module type S = sig
  module Tag : Automata.Params.TAG

  type action

  val register : action -> Tag.t 
  val get : Tag.t -> action 
end

module Make (Action : ACTION) = struct
  module Tag = struct
    type t = ..

    let compare = compare
  end

  type tag = Tag.t 
  type action = Action.t

  let table: (tag, action) Hashtbl.t = Hashtbl.create 128

  let register act =
    let module M = struct
      type Tag.t += T
    end in
    Hashtbl.add table M.T act ;
    M.T

  let get tag = Hashtbl.find table tag
end

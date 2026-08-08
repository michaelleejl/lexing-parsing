module type ACTION = sig
  type t
end

module type S = sig
  module Tag : Intfs.Tags.S

  type action
  type table

  val empty : table
  val register : action -> table -> Tag.t * table
  val get : Tag.t -> table -> action
end

module Make (Action : ACTION) () = struct
  module Tag = struct
    type t = ..

    let compare = compare
  end

  module TagMap = Map.Make (Tag)

  type action = Action.t
  type table = action TagMap.t

  let empty = TagMap.empty

  let register act tbl =
    let module M = struct
      type Tag.t += T
    end in
    (M.T, TagMap.add M.T act tbl)

  let get tag tbl = TagMap.find tag tbl
end

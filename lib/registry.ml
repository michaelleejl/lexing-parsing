module type ELT = sig
  type elt
end

module type S = sig
  module Tag : Automata.Params.TAG

  type elt

  val register : elt -> Tag.t
  val get : Tag.t -> elt
end

module Make (Elt : ELT) = struct
  module Tag = struct
    type t = ..

    let compare = compare
  end

  type tag = Tag.t
  type elt = Elt.elt

  let table : (tag, elt) Hashtbl.t = Hashtbl.create 128
  let elts = ref []

  let register elt =
    let module M = struct
      type Tag.t += T
    end in
    Hashtbl.add table M.T elt;
    elts := elt :: !elts;
    M.T

  let get tag = Hashtbl.find table tag
  let fold f = Hashtbl.fold f table
  let elts () = !elts
end

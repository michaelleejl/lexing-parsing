module type ELT = sig
  type elt
end

module type S = sig
  module Id : Id.ID

  type elt

  val register : elt -> Id.t
  val get : Id.t -> elt
end

module Make (Elt : ELT) = struct
  module Id = struct
    type t = ..

    let compare = compare
  end

  type id = Id.t
  type elt = Elt.elt

  let table : (id, elt) Hashtbl.t = Hashtbl.create 128
  let elts = ref []

  let register elt =
    let module M = struct
      type Id.t += T
    end in
    Hashtbl.add table M.T elt;
    elts := elt :: !elts;
    M.T

  let get id = Hashtbl.find table id
  let fold f = Hashtbl.fold f table
  let elts () = !elts
end

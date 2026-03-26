open Intfs.Tags

module type S = sig
  type tag

  module TaggedNfa : Tnfa.S with type tag = tag
  module StateSet : Set.S with type elt = TaggedNfa.StateSet.elt
  module StateMap : Map.S with type key = int
  module CharSet : Set.S with type elt = TaggedNfa.CharSet.elt
  module CharMap : Map.S with type key = char

  type state = StateSet.elt
  type state_set = StateSet.t
  type char_set = CharSet.t
  type transition = state CharMap.t
  type tag_lookup = tag option StateMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : char_set;
    tagger : tag_lookup;
  }

  val determinise : TaggedNfa.t -> t
  val initialise : t -> state
  val is_rejecting : t -> state -> bool
  val is_accepting : t -> state -> bool
  val step : t -> state -> char -> state
  val emit_tag : t -> state -> tag option
end

module Make (Tag : T) = struct
  module TaggedNfa = Tnfa.Make (Tag)
  module StateSet = TaggedNfa.StateSet
  module StateMap = Map.Make (Int)
  module CharSet = TaggedNfa.CharSet
  module CharMap = Map.Make (Char)

  type tag = Tag.t
  type state = StateSet.elt
  type state_set = StateSet.t
  type char_set = CharSet.t
  type transition = state CharMap.t
  type tag_lookup = tag option StateMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : char_set;
    tagger : tag_lookup;
  }

  let find_next_state next q c = CharMap.find c (next q)

  let add_transition (source, c, target) transitions =
    match StateMap.find source transitions with
    | exception Not_found ->
        StateMap.add source (CharMap.singleton c target) transitions
    | cm -> StateMap.add source (CharMap.add c target cm) transitions

  let add_tag state tag tagger = StateMap.add state tag tagger

  let initialise t_dfa = t_dfa.initial
  let is_rejecting t_dfa q = q = t_dfa.rejecting
  let is_accepting t_dfa q = StateSet.mem q t_dfa.finals

  let step t_dfa q c =
    try CharMap.find c (t_dfa.next q) with Not_found -> t_dfa.rejecting

  let emit_tag t_dfa q =
    match StateMap.find q t_dfa.tagger with
    | exception Not_found -> None
    | v -> v
end

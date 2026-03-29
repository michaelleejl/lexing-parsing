open Intfs

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

module Make (Tag : Tags.S) = struct
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

  let determinise n =
    let nfa_initial = TaggedNfa.initialise n in
    let module M = Map.Make (TaggedNfa.StateSet) in
    let gen_state =
      let next_state = ref 0 in
      fun () ->
        let s = !next_state in
        next_state := s + 1;
        s
    in
    let rec build nfa_state
        (mapping, states, transitions, finals, rejecting, tagger) =
      match M.find nfa_state mapping with
      | dfa_state ->
          (dfa_state, mapping, states, transitions, finals, rejecting, tagger)
      | exception Not_found ->
          let dfa_state = gen_state () in
          let mapping = M.add nfa_state dfa_state mapping in
          let finals =
            if TaggedNfa.is_accepting n nfa_state then
              StateSet.add dfa_state finals
            else finals
          in
          let rejecting =
            if TaggedNfa.is_rejecting n nfa_state then Some dfa_state
            else rejecting
          in
          let tagger =
            add_tag dfa_state (TaggedNfa.emit_tag n nfa_state) tagger
          in
          let states = StateSet.add dfa_state states in
          let find_next_state = TaggedNfa.step n nfa_state in
          let builder c (m, s, t, f, r, tg) =
            let next_state = find_next_state c in
            let dfa_next_state, m', s', t', f', r', tg' =
              build next_state (m, s, t, f, r, tg)
            in
            let t'' = add_transition (dfa_state, c, dfa_next_state) t' in
            let s'' = StateSet.add dfa_next_state s' in
            (m', s'', t'', f', r', tg')
          in
          let mapping', states', transitions', finals', rejecting', tagger' =
            TaggedNfa.CharSet.fold builder n.alphabet
              (mapping, states, transitions, finals, rejecting, tagger)
          in
          ( dfa_state,
            mapping',
            states',
            transitions',
            finals',
            rejecting',
            tagger' )
    in
    let initial, _, states, transitions, finals, rejecting, tagger =
      build nfa_initial
        ( M.empty,
          StateSet.empty,
          StateMap.empty,
          StateSet.empty,
          None,
          StateMap.empty )
    in
    let next s =
      try StateMap.find s transitions with Not_found -> CharMap.empty
    in
    let alphabet = n.alphabet in
    match rejecting with
    | None -> raise (Failure "Should have at least one rejecting state")
    | Some rejecting ->
        { states; initial; finals; next; alphabet; rejecting; tagger }

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

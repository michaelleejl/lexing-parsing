open Params

module type S = sig
  type tag
  type input

  module TaggedNfa : Tnfa.S with type tag = tag and type input = input
  module StateSet : Set.S with type elt = TaggedNfa.StateSet.elt
  module StateMap : Map.S with type key = int
  module InputSet : Set.S with type elt = TaggedNfa.InputSet.elt
  module InputMap : Map.S with type key = input

  type state = StateSet.elt
  type state_set = StateSet.t
  type input_set = InputSet.t
  type transition = state InputMap.t
  type tag_lookup = tag option StateMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : input_set;
    tagger : tag_lookup;
  }

  type determinisation = { dfa : t; subsets : state -> TaggedNfa.state_set }

  val subset_construction : TaggedNfa.t -> determinisation
  val determinise : TaggedNfa.t -> t
  val initialise : t -> state
  val is_rejecting : t -> state -> bool
  val is_accepting : t -> state -> bool
  val step : t -> state -> input -> state
  val emit_tag : t -> state -> tag option
end

module Make (Input : INPUT) (Tag : TAG) = struct
  type input = Input.t
  type tag = Tag.t

  module TaggedNfa : Tnfa.S with type tag = tag and type input = input =
    Tnfa.Make (Input) (Tag)

  module StateSet = TaggedNfa.StateSet
  module StateMap = Map.Make (Int)
  module InputSet = TaggedNfa.InputSet
  module InputMap = Map.Make (Input)

  type state = StateSet.elt
  type state_set = StateSet.t
  type input_set = InputSet.t
  type transition = state InputMap.t
  type tag_lookup = tag option StateMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : input_set;
    tagger : tag_lookup;
  }

  let failure = 0

  let add_transition (source, c, target) transitions =
    match StateMap.find source transitions with
    | exception Not_found ->
        StateMap.add source (InputMap.singleton c target) transitions
    | cm -> StateMap.add source (InputMap.add c target cm) transitions

  let add_tag state tag tagger = StateMap.add state tag tagger

  type determinisation = { dfa : t; subsets : state -> TaggedNfa.state_set }

  let subset_construction n =
    let nfa_initial = TaggedNfa.initialise n in
    let module M = Map.Make (TaggedNfa.StateSet) in
    let gen_state =
      let next_state = ref 1 in
      (*0 a rejecting state*)
      fun () ->
        let s = !next_state in
        next_state := s + 1;
        s
    in
    let rec build nfa_state
        (mapping, subsets, states, transitions, finals, tagger) =
      match M.find nfa_state mapping with
      | dfa_state ->
          (dfa_state, mapping, subsets, states, transitions, finals, tagger)
      | exception Not_found ->
          let dfa_state = gen_state () in
          let mapping = M.add nfa_state dfa_state mapping in
          let subsets = StateMap.add dfa_state nfa_state subsets in
          let finals =
            if TaggedNfa.is_accepting n nfa_state then
              StateSet.add dfa_state finals
            else finals
          in
          let tagger =
            add_tag dfa_state (TaggedNfa.emit_tag n nfa_state) tagger
          in
          let states = StateSet.add dfa_state states in
          let find_next_state = TaggedNfa.step n nfa_state in
          let builder c (m, su, s, t, f, tg) =
            let next_state = find_next_state c in
            let dfa_next_state, m', su', s', t', f', tg' =
              build next_state (m, su, s, t, f, tg)
            in
            let t'' = add_transition (dfa_state, c, dfa_next_state) t' in
            let s'' = StateSet.add dfa_next_state s' in
            (m', su', s'', t'', f', tg')
          in
          let mapping', subsets', states', transitions', finals', tagger' =
            TaggedNfa.InputSet.fold builder n.alphabet
              (mapping, subsets, states, transitions, finals, tagger)
          in
          ( dfa_state,
            mapping',
            subsets',
            states',
            transitions',
            finals',
            tagger' )
    in
    let initial_mapping = M.singleton TaggedNfa.StateSet.empty failure in
    let initial_subsets = StateMap.singleton failure TaggedNfa.StateSet.empty in
    let initial_states = StateSet.of_list [ failure ] in
    let initial_transitions =
      StateMap.add failure InputMap.empty StateMap.empty
    in
    let initial_tagger = StateMap.singleton failure None in
    let initial, _, subsets, states, transitions, finals, tagger =
      build nfa_initial
        ( initial_mapping,
          initial_subsets,
          initial_states,
          initial_transitions,
          StateSet.empty,
          initial_tagger )
    in
    let next s =
      try StateMap.find s transitions with Not_found -> InputMap.empty
    in
    let alphabet = n.alphabet in
    {
      dfa =
        { states; initial; finals; rejecting = failure; next; alphabet; tagger };
      subsets = (fun state -> StateMap.find state subsets);
    }

  let determinise n = (subset_construction n).dfa
  let initialise t_dfa = t_dfa.initial
  let is_rejecting t_dfa q = q = t_dfa.rejecting
  let is_accepting t_dfa q = StateSet.mem q t_dfa.finals

  let step t_dfa q c =
    try InputMap.find c (t_dfa.next q) with Not_found -> t_dfa.rejecting

  let emit_tag t_dfa q =
    match StateMap.find q t_dfa.tagger with
    | exception Not_found -> None
    | v -> v
end

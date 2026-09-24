open Params

module type S = sig
  type tag
  type input

  module Tagged_nfa : Tnfa.S with type tag = tag and type input = input
  module State_set : Set.S with type elt = Tagged_nfa.State_set.elt
  module State_map : Map.S with type key = int
  module Input_set : Set.S with type elt = Tagged_nfa.Input_set.elt
  module Input_map : Map.S with type key = input

  type state = State_set.elt
  type state_set = State_set.t
  type input_set = Input_set.t
  type transition = state Input_map.t
  type tag_lookup = tag option State_map.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : input_set;
    tagger : tag_lookup;
  }

  type determinisation = { dfa : t; subsets : state -> Tagged_nfa.state_set }

  val subset_construction : Tagged_nfa.t -> determinisation
  val determinise : Tagged_nfa.t -> t
  val initialise : t -> state
  val is_rejecting : t -> state -> bool
  val is_accepting : t -> state -> bool
  val step : t -> state -> input -> state
  val emit_tag : t -> state -> tag option
end

module Make (Input : INPUT) (Tag : TAG) = struct
  type input = Input.t
  type tag = Tag.t

  module Tagged_nfa : Tnfa.S with type tag = tag and type input = input =
    Tnfa.Make (Input) (Tag)

  module State_set = Tagged_nfa.State_set
  module State_map = Map.Make (Int)
  module Input_set = Tagged_nfa.Input_set
  module Input_map = Map.Make (Input)

  type state = State_set.elt
  type state_set = State_set.t
  type input_set = Input_set.t
  type transition = state Input_map.t
  type tag_lookup = tag option State_map.t

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
    match State_map.find source transitions with
    | exception Not_found ->
        State_map.add source (Input_map.singleton c target) transitions
    | cm -> State_map.add source (Input_map.add c target cm) transitions

  let add_tag state tag tagger = State_map.add state tag tagger

  type determinisation = { dfa : t; subsets : state -> Tagged_nfa.state_set }

  let subset_construction n =
    let nfa_initial = Tagged_nfa.initialise n in
    let module M = Map.Make (Tagged_nfa.State_set) in
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
          let subsets = State_map.add dfa_state nfa_state subsets in
          let finals =
            if Tagged_nfa.is_accepting n nfa_state then
              State_set.add dfa_state finals
            else finals
          in
          let tagger =
            add_tag dfa_state (Tagged_nfa.emit_tag n nfa_state) tagger
          in
          let states = State_set.add dfa_state states in
          let find_next_state = Tagged_nfa.step n nfa_state in
          let builder c (m, su, s, t, f, tg) =
            let next_state = find_next_state c in
            let dfa_next_state, m', su', s', t', f', tg' =
              build next_state (m, su, s, t, f, tg)
            in
            let t'' = add_transition (dfa_state, c, dfa_next_state) t' in
            let s'' = State_set.add dfa_next_state s' in
            (m', su', s'', t'', f', tg')
          in
          let mapping', subsets', states', transitions', finals', tagger' =
            Tagged_nfa.Input_set.fold builder n.alphabet
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
    let initial_mapping = M.singleton Tagged_nfa.State_set.empty failure in
    let initial_subsets =
      State_map.singleton failure Tagged_nfa.State_set.empty
    in
    let initial_states = State_set.of_list [ failure ] in
    let initial_transitions =
      State_map.add failure Input_map.empty State_map.empty
    in
    let initial_tagger = State_map.singleton failure None in
    let initial, _, subsets, states, transitions, finals, tagger =
      build nfa_initial
        ( initial_mapping,
          initial_subsets,
          initial_states,
          initial_transitions,
          State_set.empty,
          initial_tagger )
    in
    let next s =
      try State_map.find s transitions with Not_found -> Input_map.empty
    in
    let alphabet = n.alphabet in
    {
      dfa =
        { states; initial; finals; rejecting = failure; next; alphabet; tagger };
      subsets = (fun state -> State_map.find state subsets);
    }

  let determinise n = (subset_construction n).dfa
  let initialise t_dfa = t_dfa.initial
  let is_rejecting t_dfa q = q = t_dfa.rejecting
  let is_accepting t_dfa q = State_set.mem q t_dfa.finals

  let step t_dfa q c =
    try Input_map.find c (t_dfa.next q) with Not_found -> t_dfa.rejecting

  let emit_tag t_dfa q =
    match State_map.find q t_dfa.tagger with
    | exception Not_found -> None
    | v -> v
end

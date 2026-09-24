open Params

module type S = sig
  type input

  module Nfa : Nfa.S with type input = input
  module State_set : Set.S with type elt = Nfa.State_set.elt
  module State_map : Map.S with type key = int
  module Input_set : Set.S with type elt = Nfa.Input_set.elt
  module Input_map : Map.S with type key = input

  type state = State_set.elt [@@deriving compare]
  type state_set = State_set.t
  type input_set = Input_set.t
  type transition = state Input_map.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : input_set;
  }

  type determinisation = { dfa : t; subsets : state -> Nfa.state_set }

  val subset_construction : Nfa.t -> determinisation
  val determinise : Nfa.t -> t
  val initialise : t -> state
  val is_rejecting : t -> state -> bool
  val is_accepting : t -> state -> bool
  val step : t -> state -> input -> state
  val accept : t -> input list -> bool
end

module Make (Input : INPUT) = struct
  type input = Input.t

  module Nfa = Nfa.Make (Input)
  module State_set = Nfa.State_set
  module State_map = Map.Make (Int)
  module Input_set = Nfa.Input_set
  module Input_map = Map.Make (Input)
  module State = Int

  type state = State.t [@@deriving compare]
  type state_set = State_set.t
  type input_set = Input_set.t
  type transition = state Input_map.t

  let failure = 0

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    rejecting : state;
    next : state -> transition;
    alphabet : input_set;
  }

  let add_transition (source, c, target) transitions =
    match State_map.find source transitions with
    | exception Not_found ->
        State_map.add source (Input_map.singleton c target) transitions
    | cm -> State_map.add source (Input_map.add c target cm) transitions

  type determinisation = { dfa : t; subsets : state -> Nfa.state_set }

  let subset_construction n =
    let nfa_initial = Nfa.initialise n in
    let module M = Map.Make (Nfa.State_set) in
    let module S = State_map in
    let gen_state =
      let next_state = ref 1 in
      (*0 a rejecting state*)
      fun () ->
        let s = !next_state in
        next_state := s + 1;
        s
    in
    let rec build nfa_state (mapping, subsets, states, transitions, finals) =
      match M.find nfa_state mapping with
      | dfa_state -> (dfa_state, mapping, subsets, states, transitions, finals)
      | exception Not_found ->
          let dfa_state = gen_state () in
          let mapping = M.add nfa_state dfa_state mapping in
          let subsets = S.add dfa_state nfa_state subsets in
          let finals =
            if Nfa.is_accepting n nfa_state then State_set.add dfa_state finals
            else finals
          in
          let states = State_set.add dfa_state states in
          let find_next_state = Nfa.step n nfa_state in
          let builder c (m, su, st, t, f) =
            let next_state = find_next_state c in
            let dfa_next_state, m', su', st', t', f' =
              build next_state (m, su, st, t, f)
            in
            let t'' = add_transition (dfa_state, c, dfa_next_state) t' in
            let s'' = State_set.add dfa_next_state st' in
            (m', su', s'', t'', f')
          in
          let mapping', subsets', states', transitions', finals' =
            Nfa.Input_set.fold builder n.alphabet
              (mapping, subsets, states, transitions, finals)
          in
          (dfa_state, mapping', subsets', states', transitions', finals')
    in
    let initial_mapping = M.singleton State_set.empty failure in
    let inital_transitions = State_map.add 0 Input_map.empty State_map.empty in
    let initial_states = State_set.of_list [ 0 ] in
    let initial_subsets = S.singleton 0 Nfa.State_set.empty in
    let initial, _, subsets, states, transitions, finals =
      build nfa_initial
        ( initial_mapping,
          initial_subsets,
          initial_states,
          inital_transitions,
          State_set.empty )
    in
    let next s =
      try State_map.find s transitions with Not_found -> Input_map.empty
    in
    let alphabet = n.alphabet in
    {
      dfa = { states; initial; finals; rejecting = failure; next; alphabet };
      subsets = (fun state -> S.find state subsets);
    }

  let determinise n = (subset_construction n).dfa
  let initialise dfa = dfa.initial
  let is_rejecting dfa q = q = dfa.rejecting
  let is_accepting dfa q = State_set.mem q dfa.finals

  let step dfa q c =
    try Input_map.find c (dfa.next q) with Not_found -> dfa.rejecting

  let accept dfa xs =
    List.fold_left (step dfa) dfa.initial xs |> is_accepting dfa
end

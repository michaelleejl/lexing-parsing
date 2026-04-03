open Intfs 

module type S = sig 
  
  type input 
  module Nfa : Nfa.S with type input = input 

  module StateSet : Set.S with type elt = Nfa.StateSet.elt
  module StateMap : Map.S with type key = int
  module InputSet : Set.S with type elt = Nfa.InputSet.elt
  module InputMap : Map.S with type key = input

  type state = StateSet.elt
  type state_set = StateSet.t
  type input_set = InputSet.t
  type transition = state InputMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
  }

  val determinise : Nfa.t -> t
  val accept : t -> input list -> bool

end 

module Make (Input: Inputs.S) = struct 
  type input = Input.t 

  module Nfa = Nfa.Make(Input)
  module StateSet = Nfa.StateSet
  module StateMap = Map.Make (Int)
  module InputSet = Nfa.InputSet
  module InputMap = Map.Make (Input)

  type state = StateSet.elt
  type state_set = StateSet.t
  type input_set = InputSet.t
  type transition = state InputMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
  }

  let find_next_state next q c = InputMap.find c (next q)

  let add_transition (source, c, target) transitions =
    match StateMap.find source transitions with
    | exception Not_found ->
        StateMap.add source (InputMap.singleton c target) transitions
    | cm -> StateMap.add source (InputMap.add c target cm) transitions

  let determinise n =
    let nfa_initial = Nfa.initialise n in
    let module M = Map.Make (Nfa.StateSet) in
    let gen_state =
      let next_state = ref 0 in
      fun () ->
        let s = !next_state in
        next_state := s + 1;
        s
    in
    let rec build nfa_state (mapping, states, transitions, finals) =
      match M.find nfa_state mapping with
      | dfa_state -> (dfa_state, mapping, states, transitions, finals)
      | exception Not_found ->
          let dfa_state = gen_state () in
          let mapping = M.add nfa_state dfa_state mapping in
          let finals =
            if Nfa.is_accepting n nfa_state then StateSet.add dfa_state finals
            else finals
          in
          let states = StateSet.add dfa_state states in
          let find_next_state = Nfa.step n nfa_state in
          let builder c (m, s, t, f) =
            let next_state = find_next_state c in
            let dfa_next_state, m', s', t', f' = build next_state (m, s, t, f) in
            let t'' = add_transition (dfa_state, c, dfa_next_state) t' in
            let s'' = StateSet.add dfa_next_state s' in
            (m', s'', t'', f')
          in
          let mapping', states', transitions', finals' =
            Nfa.InputSet.fold builder n.alphabet
              (mapping, states, transitions, finals)
          in
          (dfa_state, mapping', states', transitions', finals')
    in
    let initial, _, states, transitions, finals =
      build nfa_initial (M.empty, StateSet.empty, StateMap.empty, StateSet.empty)
    in
    let next s =
      try StateMap.find s transitions with Not_found -> InputMap.empty
    in
    let alphabet = n.alphabet in
    { states; initial; finals; next; alphabet }

  let accept d xs =
    let next_state = find_next_state d.next in
    let final = List.fold_left (fun q -> fun c -> next_state q c) d.initial xs in
    StateSet.mem final d.finals 
end 

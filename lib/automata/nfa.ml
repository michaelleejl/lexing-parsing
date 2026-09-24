open Params

module type S = sig
  type state = int
  type input

  module State = Int
  module State_set : Set.S with type elt = state
  module Input_set : Set.S with type elt = input
  module Input_opt_map : Map.S with type key = input option

  type transition = State_set.t Input_opt_map.t
  type state_set = State_set.t
  type input_set = Input_set.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
  }

  val init :
    state_set -> state -> state_set -> (state -> transition) -> input_set -> t

  val empty : t
  val epsilon : t
  val one_of : ?alphabet:input_set -> input list -> t
  val alt : t -> t -> t
  val seq : t -> t -> t
  val kleene : t -> t
  val accept : t -> input list -> bool
  val initialise : t -> state_set
  val is_accepting : t -> state_set -> bool
  val is_rejecting : t -> state_set -> bool
  val step : t -> state_set -> input -> state_set
end

module Make (Input : INPUT) = struct
  module State = Int
  module State_set = Set.Make (State)
  module Input_set = Set.Make (Input)

  type input = Input.t

  module Input_opt = struct
    type t = input option

    let compare = compare
  end

  module Input_opt_map = Map.Make (Input_opt)

  type state = State_set.elt
  type state_set = State_set.t
  type transition = state_set Input_opt_map.t
  type input_set = Input_set.t

  let merge _ x y = Some (State_set.union x y)

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
  }

  let init states initial finals next alphabet =
    { states; initial; finals; next; alphabet }

  let rn_shift ?(m = 1) n =
    {
      states = State_set.map (fun s -> s + m) n.states;
      initial = n.initial + m;
      finals = State_set.map (( + ) m) n.finals;
      next =
        (fun s ->
          Input_opt_map.map
            (fun v -> State_set.map (fun x -> x + m) v)
            (n.next (s - m)));
      alphabet = n.alphabet;
    }

  let rn_even n =
    {
      states = State_set.map (fun s -> s * 2) n.states;
      initial = n.initial * 2;
      finals = State_set.map (fun s -> s * 2) n.finals;
      next =
        (fun s ->
          Input_opt_map.map
            (fun v -> State_set.map (fun x -> x * 2) v)
            (n.next (s / 2)));
      alphabet = n.alphabet;
    }

  let rn_odd n =
    {
      states = State_set.map (fun s -> (s * 2) + 1) n.states;
      initial = (n.initial * 2) + 1;
      finals = State_set.map (fun s -> (s * 2) + 1) n.finals;
      next =
        (fun s ->
          Input_opt_map.map
            (fun v -> State_set.map (fun x -> (x * 2) + 1) v)
            (n.next ((s - 1) / 2)));
      alphabet = n.alphabet;
    }

  let empty =
    {
      states = State_set.of_list [ 0; 1 ];
      initial = 0;
      finals = State_set.singleton 1;
      next = (fun _ -> Input_opt_map.empty);
      alphabet = Input_set.empty;
    }

  let epsilon =
    {
      states = State_set.of_list [ 0; 1 ];
      initial = 0;
      finals = State_set.singleton 1;
      next =
        (fun s ->
          if s = 0 then Input_opt_map.singleton None (State_set.singleton 1)
          else Input_opt_map.empty);
      alphabet = Input_set.empty;
    }

  let one_of ?alphabet cs =
    {
      states = State_set.of_list [ 0; 1 ];
      initial = 0;
      finals = State_set.singleton 1;
      next =
        (fun s ->
          if s = 0 then
            Input_opt_map.of_list
              (List.map (fun x -> (Some x, State_set.singleton 1)) cs)
          else Input_opt_map.empty);
      alphabet =
        (match alphabet with Some a -> a | None -> Input_set.of_list cs);
    }

  let alt n0 n1 =
    let n0' = rn_even (rn_shift ~m:2 n0) in
    let n1' = rn_odd (rn_shift ~m:2 n1) in
    let states =
      State_set.union
        (State_set.union n0'.states n1'.states)
        (State_set.of_list [ 0; 1 ])
    in
    let initials = State_set.of_list [ n0'.initial; n1'.initial ] in
    let finals = State_set.union n0'.finals n1'.finals in
    let next' = fun s -> if s mod 2 = 0 then n0'.next s else n1'.next s in
    let next =
     fun s ->
      if s = 0 then Input_opt_map.singleton None initials
      else
        let m = next' s in
        if State_set.mem s finals then
          Input_opt_map.union merge m
            (Input_opt_map.singleton None (State_set.singleton 1))
        else m
    in
    let alphabet = Input_set.union n0'.alphabet n1'.alphabet in
    { states; initial = 0; finals = State_set.singleton 1; next; alphabet }

  let seq n0 n1 =
    let n0' = rn_even n0 in
    let n1' = rn_odd n1 in
    let states = State_set.union n0'.states n1'.states in
    let alphabet = Input_set.union n0'.alphabet n1'.alphabet in
    {
      states;
      initial = n0'.initial;
      finals = n1'.finals;
      next =
        (fun s ->
          let m = if s mod 2 = 0 then n0'.next s else n1'.next s in
          if State_set.mem s n0'.finals then
            Input_opt_map.union merge m
              (Input_opt_map.singleton None (State_set.singleton n1'.initial))
          else m);
      alphabet;
    }

  let kleene n =
    let n' = rn_shift ~m:2 n in
    let states = State_set.union (State_set.of_list [ 0; 1 ]) n'.states in
    {
      states;
      initial = 0;
      finals = State_set.singleton 1;
      next =
        (fun s ->
          if s = 0 then
            Input_opt_map.singleton None (State_set.of_list [ n'.initial; 1 ])
          else if s = 1 then
            Input_opt_map.singleton None (State_set.singleton 0)
          else
            let m = n'.next s in
            if State_set.mem s n'.finals then
              Input_opt_map.union merge
                (Input_opt_map.singleton None
                   (State_set.of_list [ n'.initial; 1 ]))
                m
            else m);
      alphabet = n.alphabet;
    }

  let epsilon_step next q =
    try Input_opt_map.find None (next q) with Not_found -> State_set.empty

  let epsilon_steps next qs =
    State_set.fold
      (fun q -> fun acc -> State_set.union acc (epsilon_step next q))
      qs qs

  let epsilon_closure n =
    Fixpoint.fix ~eq:State_set.equal (epsilon_steps n.next)

  let initialise n = epsilon_closure n (State_set.singleton n.initial)

  let char_step n q c =
    try Input_opt_map.find (Some c) (n.next q)
    with Not_found -> State_set.empty

  let step n qs c =
    let next_states =
      State_set.fold
        (fun q -> fun acc -> State_set.union acc (char_step n q c))
        qs State_set.empty
    in
    epsilon_closure n next_states

  let is_final n q = State_set.mem q n.finals
  let is_rejecting n qs = State_set.is_empty qs

  let is_accepting n qs =
    State_set.fold (fun q acc -> is_final n q || acc) qs false

  let accept n xs =
    let es = List.fold_left (step n) (initialise n) xs in
    is_accepting n es
end

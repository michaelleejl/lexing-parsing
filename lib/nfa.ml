open Intfs 

module type S = sig 
  type state = int
  type input

  module State = Int
  module StateSet : Set.S with type elt = state
  module InputSet : Set.S with type elt = input
  module InputOptMap : Map.S with type key = input option

  type transition = StateSet.t InputOptMap.t
  type state_set = StateSet.t
  type input_set = InputSet.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
  }

  val empty : t
  val epsilon : t
  val one_of : input list -> t
  val alt : t -> t -> t
  val seq : t -> t -> t
  val kleene : t -> t
  val accept : t -> input list -> bool
  val initialise : t -> state_set
  val is_accepting : t -> state_set -> bool
  val is_rejecting : t -> state_set -> bool
  val step : t -> state_set -> input -> state_set
end 

module Make(Input: Inputs.S) = struct 
  module State = Int
  module StateSet = Set.Make (State)
  module InputSet = Set.Make (Input)

  type input = Input.t 
  module InputOpt = struct
    type t = input option

    let compare = compare
  end

  module InputOptMap = Map.Make (InputOpt)

  type state = StateSet.elt
  type state_set = StateSet.t
  type transition = state_set InputOptMap.t
  type input_set = InputSet.t

  let merge _ x y = Some (StateSet.union x y)

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
  }

  let rn_shift ?(m = 1) n =
    {
      states = StateSet.map (fun s -> s + m) n.states;
      initial = n.initial + m;
      finals = StateSet.map (( + ) m) n.finals;
      next =
        (fun s ->
          InputOptMap.map
            (fun v -> StateSet.map (fun x -> x + m) v)
            (n.next (s - m)));
      alphabet = n.alphabet;
    }

  let rn_even n =
    {
      states = StateSet.map (fun s -> s * 2) n.states;
      initial = n.initial * 2;
      finals = StateSet.map (fun s -> s * 2) n.finals;
      next =
        (fun s ->
          InputOptMap.map
            (fun v -> StateSet.map (fun x -> x * 2) v)
            (n.next (s / 2)));
      alphabet = n.alphabet;
    }

  let rn_odd n =
    {
      states = StateSet.map (fun s -> (s * 2) + 1) n.states;
      initial = (n.initial * 2) + 1;
      finals = StateSet.map (fun s -> (s * 2) + 1) n.finals;
      next =
        (fun s ->
          InputOptMap.map
            (fun v -> StateSet.map (fun x -> (x * 2) + 1) v)
            (n.next ((s - 1) / 2)));
      alphabet = n.alphabet;
    }

  let empty =
    {
      states = StateSet.of_list [ 0; 1 ];
      initial = 0;
      finals = StateSet.singleton 1;
      next = (fun _ -> InputOptMap.empty);
      alphabet = InputSet.empty;
    }

  let epsilon =
    {
      states = StateSet.of_list [ 0; 1 ];
      initial = 0;
      finals = StateSet.singleton 1;
      next =
        (fun s ->
          if s = 0 then InputOptMap.singleton None (StateSet.singleton 1)
          else InputOptMap.empty);
      alphabet = InputSet.empty;
    }

  let one_of cs =
    {
      states = StateSet.of_list [ 0; 1 ];
      initial = 0;
      finals = StateSet.singleton 1;
      next =
        (fun s ->
          if s = 0 then
            InputOptMap.of_list
              (List.map (fun x -> (Some x, StateSet.singleton 1)) cs)
          else InputOptMap.empty);
      alphabet = InputSet.of_list cs;
    }

  let alt n0 n1 =
    let n0' = rn_even (rn_shift ~m:2 n0) in
    let n1' = rn_odd (rn_shift ~m:2 n1) in
    let states =
      StateSet.union
        (StateSet.union n0'.states n1'.states)
        (StateSet.of_list [ 0; 1 ])
    in
    let initials = StateSet.of_list [ n0'.initial; n1'.initial ] in
    let finals = StateSet.union n0'.finals n1'.finals in
    let next' = fun s -> if s mod 2 = 0 then n0'.next s else n1'.next s in
    let next =
    fun s ->
      if s = 0 then InputOptMap.singleton None initials
      else
        let m = next' s in
        if StateSet.mem s finals then
          InputOptMap.union merge m
            (InputOptMap.singleton None (StateSet.singleton 1))
        else m
    in
    let alphabet = InputSet.union n0'.alphabet n1'.alphabet in
    { states; initial = 0; finals = StateSet.singleton 1; next; alphabet }

  let seq n0 n1 =
    let n0' = rn_even n0 in
    let n1' = rn_odd n1 in
    let states = StateSet.union n0'.states n1'.states in
    let alphabet = InputSet.union n0'.alphabet n1'.alphabet in
    {
      states;
      initial = n0'.initial;
      finals = n1'.finals;
      next =
        (fun s ->
          let m = if s mod 2 = 0 then n0'.next s else n1'.next s in
          if StateSet.mem s n0'.finals then
            InputOptMap.union merge m
              (InputOptMap.singleton None (StateSet.singleton n1'.initial))
          else m);
      alphabet;
    }

  let kleene n =
    let n' = rn_shift ~m:2 n in
    let states = StateSet.union (StateSet.of_list [ 0; 1 ]) n'.states in
    {
      states;
      initial = 0;
      finals = StateSet.singleton 1;
      next =
        (fun s ->
          if s = 0 then
            InputOptMap.singleton None (StateSet.of_list [ n'.initial; 1 ])
          else if s = 1 then InputOptMap.singleton None (StateSet.singleton 0)
          else
            let m = n'.next s in
            if StateSet.mem s n'.finals then
              InputOptMap.union merge
                (InputOptMap.singleton None (StateSet.of_list [ n'.initial; 1 ]))
                m
            else m);
      alphabet = n.alphabet;
    }

  let epsilon_step next q =
    try InputOptMap.find None (next q) with Not_found -> StateSet.empty

  let epsilon_steps next qs =
    StateSet.fold
      (fun q -> fun acc -> StateSet.union acc (epsilon_step next q))
      qs qs

  let rec epsilon_closure n qs =
    let qs' = epsilon_steps n.next qs in
    if StateSet.equal qs' qs then qs else epsilon_closure n qs'

  let initialise n = epsilon_closure n (StateSet.singleton n.initial)

  let char_step n q c =
    try InputOptMap.find (Some c) (n.next q) with Not_found -> StateSet.empty

  let step n qs c =
    let next_states =
      StateSet.fold
        (fun q -> fun acc -> StateSet.union acc (char_step n q c))
        qs StateSet.empty
    in
    epsilon_closure n next_states

  let is_final n q = StateSet.mem q n.finals
  let is_rejecting n qs = StateSet.is_empty qs

  let is_accepting n qs =
    StateSet.fold (fun q acc -> is_final n q || acc) qs false

  let accept n xs =
    let es = List.fold_left (step n) (initialise n) xs in
    is_accepting n es
end 
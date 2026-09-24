open Params

module type S = sig
  type input

  module Nfa : Nfa.S with type input = input

  type state = Nfa.state

  module State_set : Set.S with type elt = state
  module Input_set : Set.S with type elt = input
  module Input_opt_map : Map.S with type key = input option
  module State_map : Map.S with type key = state

  type tag
  type transition = State_set.t Input_opt_map.t
  type state_set = State_set.t
  type input_set = Input_set.t
  type tag_lookup = tag State_map.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
    tagger : tag_lookup;
  }

  val lift : Nfa.t -> tag -> t
  val alt : t -> t -> t
  val initialise : t -> state_set
  val is_rejecting : t -> state_set -> bool
  val is_accepting : t -> state_set -> bool
  val step : t -> state_set -> input -> state_set
  val emit_tag : t -> state_set -> tag option
end

module Make (Input : INPUT) (Tag : TAG) = struct
  module Nfa = Nfa.Make (Input)
  include Nfa
  module State_map = Map.Make (State)

  type tag = Tag.t
  type tag_lookup = tag State_map.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : input_set;
    tagger : tag_lookup;
  }

  let merge _ x y = Some (State_set.union x y)

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
      tagger =
        State_map.fold
          (fun k v new_map -> State_map.add (k + m) v new_map)
          n.tagger State_map.empty;
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
      tagger =
        State_map.fold
          (fun k v new_map -> State_map.add (2 * k) v new_map)
          n.tagger State_map.empty;
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
      tagger =
        State_map.fold
          (fun k v new_map -> State_map.add ((2 * k) + 1) v new_map)
          n.tagger State_map.empty;
    }

  let lift ({ states; initial; finals; next; alphabet } : Nfa.t) tag =
    let tagger =
      State_map.of_seq @@ List.to_seq
      @@ List.map (fun s -> (s, tag)) (State_set.to_list finals)
    in
    { states; initial; finals; next; alphabet; tagger }

  let alt tn0 tn1 =
    let tn0' = rn_even (rn_shift ~m:2 tn0) in
    let tn1' = rn_odd (rn_shift ~m:2 tn1) in
    let states =
      State_set.union
        (State_set.union tn0'.states tn1'.states)
        (State_set.of_list [ 0; 1 ])
    in
    let initials = State_set.of_list [ tn0'.initial; tn1'.initial ] in
    let finals = State_set.union tn0'.finals tn1'.finals in
    let next' = fun s -> if s mod 2 = 0 then tn0'.next s else tn1'.next s in
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
    let alphabet = Input_set.union tn0'.alphabet tn1'.alphabet in
    let tagger =
      State_map.union
        (fun _ t1 t2 -> if t1 < t2 then Some t1 else Some t2)
        tn0'.tagger tn1'.tagger
    in
    {
      states;
      initial = 0;
      finals = State_set.singleton 1;
      next;
      alphabet;
      tagger;
    }

  let to_nfa { states; initial; finals; next; alphabet } =
    { states; initial; finals; next; alphabet }

  let initialise t = to_nfa t |> Nfa.initialise
  let is_rejecting t = to_nfa t |> Nfa.is_rejecting
  let is_accepting t = to_nfa t |> Nfa.is_accepting
  let step t = to_nfa t |> Nfa.step

  let emit_tag t states =
    let state_list = State_set.to_list states in
    List.fold_left
      (fun acc s ->
        match State_map.find s t.tagger with
        | exception Not_found -> acc
        | v -> (
            match acc with
            | None -> Some v
            | Some v' -> if v < v' then Some v else acc))
      None state_list
end

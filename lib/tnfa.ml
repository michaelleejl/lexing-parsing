open Intfs.Tags

module type S = sig 
  type state = Nfa.state

  module StateSet : Set.S with type elt = state
  module CharSet : Set.S with type elt = char
  module CharOptMap : Map.S with type key = char option
  module StateMap : Map.S with type key = state

  type tag 

  type transition = StateSet.t CharOptMap.t
  type state_set = StateSet.t
  type char_set = CharSet.t
  type tag_lookup = tag StateMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : char_set;
    tagger : tag_lookup;
  }

  val lift : Nfa.t -> tag -> t
  val alt : t -> t -> t
  val initialise : t -> state_set
  val is_rejecting : t -> state_set -> bool
  val is_accepting : t -> state_set -> bool
  val step : t -> state_set -> char -> state_set
  val emit_tag : t -> state_set -> tag option

end

module Make (Tag : T) = struct
  include Nfa
  module StateMap = Map.Make (State)

  type tag = Tag.t
  type tag_lookup = tag StateMap.t

  type t = {
    states : state_set;
    initial : state;
    finals : state_set;
    next : state -> transition;
    alphabet : char_set;
    tagger : tag_lookup;
  }

  let merge _ x y = Some (StateSet.union x y)

  let rn_shift ?(m = 1) n =
    {
      states = StateSet.map (fun s -> s + m) n.states;
      initial = n.initial + m;
      finals = StateSet.map (( + ) m) n.finals;
      next =
        (fun s ->
          CharOptMap.map
            (fun v -> StateSet.map (fun x -> x + m) v)
            (n.next (s - m)));
      alphabet = n.alphabet;
      tagger =
        StateMap.fold
          (fun k v new_map -> StateMap.add (k + m) v new_map)
          n.tagger StateMap.empty;
    }

  let rn_even n =
    {
      states = StateSet.map (fun s -> s * 2) n.states;
      initial = n.initial * 2;
      finals = StateSet.map (fun s -> s * 2) n.finals;
      next =
        (fun s ->
          CharOptMap.map
            (fun v -> StateSet.map (fun x -> x * 2) v)
            (n.next (s / 2)));
      alphabet = n.alphabet;
      tagger =
        StateMap.fold
          (fun k v new_map -> StateMap.add (2 * k) v new_map)
          n.tagger StateMap.empty;
    }

  let rn_odd n =
    {
      states = StateSet.map (fun s -> (s * 2) + 1) n.states;
      initial = (n.initial * 2) + 1;
      finals = StateSet.map (fun s -> (s * 2) + 1) n.finals;
      next =
        (fun s ->
          CharOptMap.map
            (fun v -> StateSet.map (fun x -> (x * 2) + 1) v)
            (n.next ((s - 1) / 2)));
      alphabet = n.alphabet;
      tagger =
        StateMap.fold
          (fun k v new_map -> StateMap.add ((2 * k) + 1) v new_map)
          n.tagger StateMap.empty;
    }

  let lift ({ states; initial; finals; next; alphabet } : Nfa.t) tag =
    let tagger =
      StateMap.of_seq @@ List.to_seq
      @@ List.map (fun s -> (s, tag)) (StateSet.to_list states)
    in
    { states; initial; finals; next; alphabet; tagger }

  let alt tn0 tn1 =
    let tn0' = rn_even (rn_shift ~m:2 tn0) in
    let tn1' = rn_odd (rn_shift ~m:2 tn1) in
    let states =
      StateSet.union
        (StateSet.union tn0'.states tn1'.states)
        (StateSet.of_list [ 0; 1 ])
    in
    let initials = StateSet.of_list [ tn0'.initial; tn1'.initial ] in
    let finals = StateSet.union tn0'.finals tn1'.finals in
    let next' = fun s -> if s mod 2 = 0 then tn0'.next s else tn1'.next s in
    let next =
     fun s ->
      if s = 0 then CharOptMap.singleton None initials
      else
        let m = next' s in
        if StateSet.mem s finals then
          CharOptMap.union merge m
            (CharOptMap.singleton None (StateSet.singleton 1))
        else m
    in
    let alphabet = CharSet.union tn0'.alphabet tn1'.alphabet in
    let tagger =
      StateMap.union
        (fun _ t1 t2 -> if t1 < t2 then Some t1 else Some t2)
        tn0'.tagger tn1'.tagger
    in
    {
      states;
      initial = 0;
      finals = StateSet.singleton 1;
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

  let emit_tag { tagger } states =
    let state_list = StateSet.to_list states in
    List.fold_left
      (fun acc s ->
        match StateMap.find s tagger with
        | exception Not_found -> acc
        | v -> (
            match acc with
            | None -> Some v
            | Some v' -> if v < v' then Some v else acc))
      None state_list
end

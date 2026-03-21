module State = Int
module StateSet = Set.Make (State)

module CharOpt = struct
  type t = char option

  let compare = compare
end

module CharOptMap = Map.Make (CharOpt)

type state = StateSet.elt
type state_set = StateSet.t
type transition = state_set CharOptMap.t

let merge _ x y = Some (StateSet.union x y)

type nfa = { initial : state; finals : state_set; next : state -> transition }

let rn_shift ?(m = 1) n =
  {
    initial = n.initial + m;
    finals = StateSet.map (( + ) m) n.finals;
    next =
      (fun s ->
        CharOptMap.map (fun v -> StateSet.map (fun x -> x + m) v) (n.next (s - m)));
  }

let rn_even n =
  {
    initial = n.initial * 2;
    finals = StateSet.map (fun s -> s * 2) n.finals;
    next =
      (fun s ->
        CharOptMap.map (fun v -> StateSet.map (fun x -> x * 2) v) (n.next (s / 2)));
  }

let rn_odd n =
  {
    initial = (n.initial * 2) + 1;
    finals = StateSet.map (fun s -> (s * 2) + 1) n.finals;
    next =
      (fun s ->
        CharOptMap.map
          (fun v -> StateSet.map (fun x -> (x * 2) + 1) v)
          (n.next ((s - 1) / 2)));
  }

let empty =
  { initial = 0; finals = StateSet.singleton 1; next = (fun _ -> CharOptMap.empty) }

let epsilon =
  {
    initial = 0;
    finals = StateSet.singleton 1;
    next =
      (fun s ->
        if s = 0 then CharOptMap.singleton None (StateSet.singleton 1)
        else CharOptMap.empty);
  }

let one_of cs =
  {
    initial = 0;
    finals = StateSet.singleton 1;
    next =
      (fun s ->
        if s = 0 then
          CharOptMap.of_list
            (List.map (fun x -> (Some x, StateSet.singleton 1)) cs)
        else CharOptMap.empty);
  }

let alt n0 n1 =
  let n0' = rn_even (rn_shift ~m:2 n0) in
  let n1' = rn_odd (rn_shift ~m:2 n1) in
  let initials = StateSet.of_list [ n0'.initial; n1'.initial ] in
  let finals = StateSet.union n0'.finals n1'.finals in
  let next' = fun s -> if s mod 2 = 0 then n0'.next s else n1'.next s in
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
  { initial = 0; finals = StateSet.singleton 1; next }

let seq n0 n1 =
  let n0' = rn_even n0 in
  let n1' = rn_odd n1 in
  {
    initial = n0'.initial;
    finals = n1'.finals;
    next =
      (fun s ->
        let m = if s mod 2 = 0 then n0'.next s else n1'.next s in
        if StateSet.mem s n0'.finals then
          CharOptMap.union merge m
            (CharOptMap.singleton None (StateSet.singleton n1'.initial))
        else m);
  }

let kleene n =
  let n' = rn_shift ~m:2 n in
  {
    initial = 0;
    finals = StateSet.singleton 1;
    next =
      (fun s ->
        if s = 0 then CharOptMap.singleton None (StateSet.of_list [ n'.initial; 1 ])
        else if s = 1 then CharOptMap.singleton None (StateSet.singleton 0)
        else
          let m = n'.next s in
          if StateSet.mem s n'.finals then
            CharOptMap.union merge
              (CharOptMap.singleton None (StateSet.of_list [ n'.initial; 1 ]))
              m
          else m);
  }

let epsilon_step next q =
  try CharOptMap.find None (next q) with Not_found -> StateSet.empty

let epsilon_steps next qs =
  StateSet.fold
    (fun q -> fun acc -> StateSet.union acc (epsilon_step next q))
    qs qs

let rec epsilon_closure next qs =
  let qs' = epsilon_steps next qs in
  if StateSet.equal qs' qs then qs else epsilon_closure next qs'

let char_step next q c =
  try CharOptMap.find (Some c) (next q) with Not_found -> StateSet.empty

let step next qs c =
  let curr_states = epsilon_closure next qs in
  StateSet.fold
    (fun q -> fun acc -> StateSet.union acc (char_step next q c))
    curr_states StateSet.empty

let accept n s =
  let cs = Base.String.to_list s in
  let es = List.fold_left (step n.next) (StateSet.singleton n.initial) cs in
  let end_states = epsilon_closure n.next es in
  StateSet.fold (fun q -> fun acc -> StateSet.mem q n.finals || acc) end_states false

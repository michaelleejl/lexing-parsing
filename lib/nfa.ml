module State = Int
module StateSet = Set.Make (State)
module CharSet = Set.Make (Char)

module CharOpt = struct
  type t = char option

  let compare = compare
end

module CharOptMap = Map.Make (CharOpt)

type state = StateSet.elt
type state_set = StateSet.t
type transition = state_set CharOptMap.t
type char_set = CharSet.t

let merge _ x y = Some (StateSet.union x y)

type t = {
  initial : state;
  finals : state_set;
  next : state -> transition;
  alphabet : char_set;
}

let rn_shift ?(m = 1) n =
  {
    initial = n.initial + m;
    finals = StateSet.map (( + ) m) n.finals;
    next =
      (fun s ->
        CharOptMap.map
          (fun v -> StateSet.map (fun x -> x + m) v)
          (n.next (s - m)));
    alphabet = n.alphabet;
  }

let rn_even n =
  {
    initial = n.initial * 2;
    finals = StateSet.map (fun s -> s * 2) n.finals;
    next =
      (fun s ->
        CharOptMap.map
          (fun v -> StateSet.map (fun x -> x * 2) v)
          (n.next (s / 2)));
    alphabet = n.alphabet;
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
    alphabet = n.alphabet;
  }

let empty =
  {
    initial = 0;
    finals = StateSet.singleton 1;
    next = (fun _ -> CharOptMap.empty);
    alphabet = CharSet.empty;
  }

let epsilon =
  {
    initial = 0;
    finals = StateSet.singleton 1;
    next =
      (fun s ->
        if s = 0 then CharOptMap.singleton None (StateSet.singleton 1)
        else CharOptMap.empty);
    alphabet = CharSet.empty;
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
    alphabet = CharSet.of_list cs;
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
  let alphabet = CharSet.union n0'.alphabet n1'.alphabet in
  { initial = 0; finals = StateSet.singleton 1; next; alphabet }

let seq n0 n1 =
  let n0' = rn_even n0 in
  let n1' = rn_odd n1 in
  let alphabet = CharSet.union n0'.alphabet n1'.alphabet in
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
    alphabet;
  }

let kleene n =
  let n' = rn_shift ~m:2 n in
  {
    initial = 0;
    finals = StateSet.singleton 1;
    next =
      (fun s ->
        if s = 0 then
          CharOptMap.singleton None (StateSet.of_list [ n'.initial; 1 ])
        else if s = 1 then CharOptMap.singleton None (StateSet.singleton 0)
        else
          let m = n'.next s in
          if StateSet.mem s n'.finals then
            CharOptMap.union merge
              (CharOptMap.singleton None (StateSet.of_list [ n'.initial; 1 ]))
              m
          else m);
    alphabet = n.alphabet;
  }

let epsilon_step next q =
  try CharOptMap.find None (next q) with Not_found -> StateSet.empty

let epsilon_steps next qs =
  StateSet.fold
    (fun q -> fun acc -> StateSet.union acc (epsilon_step next q))
    qs qs

let rec epsilon_closure n qs =
  let qs' = epsilon_steps n.next qs in
  if StateSet.equal qs' qs then qs else epsilon_closure n qs'

let initialise n = epsilon_closure n (StateSet.singleton n.initial)

let char_step n q c =
  try CharOptMap.find (Some c) (n.next q) with Not_found -> StateSet.empty

let step n qs c =
  let next_states =
    StateSet.fold
      (fun q -> fun acc -> StateSet.union acc (char_step n q c))
      qs StateSet.empty
  in
  epsilon_closure n next_states

let is_final n q = StateSet.mem q n.finals

let contains_final n qs =
  StateSet.fold (fun q acc -> is_final n q || acc) qs false

let accept n s =
  let cs = Base.String.to_list s in
  let es = List.fold_left (step n) (initialise n) cs in
  contains_final n es

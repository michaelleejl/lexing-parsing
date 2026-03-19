module State = Int
module StateSet = Set.Make(State)

module CharOpt = struct 
  type t = char option 
  let compare = compare 
end 

module CharOptMap = Map.Make(CharOpt)

type state = StateSet.elt 
type state_set = StateSet.t 
type transition = state_set CharOptMap.t 

let merge _ x y = Some(StateSet.union x y)

type nfa = {
  q0: state;
  f: state_set;
  d: state -> transition;
}

let rn_shift ?(m=1) n = {
  q0 = n.q0 + m;
  f  = StateSet.map ((+) m) n.f;
  d  = fun s -> n.d (s - m);
}

let rn_even n = {
  q0 = n.q0 * 2;
  f  = StateSet.map (fun s -> s * 2) n.f;
  d  = fun s -> n.d (s / 2);
}

let rn_odd n = {
  q0 = (n.q0 * 2)+1;
  f  = StateSet.map (fun s -> (s * 2) + 1) n.f;
  d  = fun s -> n.d ((s-1) / 2);
}

let empty = {
  q0 = 0;
  f = StateSet.singleton 1;
  d = fun _ -> CharOptMap.empty;
}

let epsilon = {
  q0 = 0;
  f = StateSet.singleton 1;
  d = fun s -> if s = 0 then 
                CharOptMap.singleton None (StateSet.singleton 1) 
                else CharOptMap.empty;
}

let single c = {
  q0 = 0;
  f = StateSet.singleton 1;
  d = fun s -> if s = 0 then 
                CharOptMap.singleton (Some c) (StateSet.singleton 1) 
                else CharOptMap.empty;
}

let alt n0 n1 = 
  let n0' = rn_even (rn_shift ~m:2 n0) in 
  let n1' = rn_odd (rn_shift ~m:2 n1) in 
  let initials = StateSet.of_list [ n0'.q0 ; n1'.q0 ] in 
  let finals = StateSet.union (n0'.f) (n1'.f) in 
  let d' = (fun s -> 
            if s mod 2 = 0 then n0'.d s 
            else n1'.d s)  in 
  let d = (fun s -> 
          if s = 0 then 
            CharOptMap.singleton None initials
          else 
            let m = d' s in 
            (if StateSet.mem s finals then 
              CharOptMap.union merge m
              (CharOptMap.singleton None (StateSet.singleton 1)) 
            else m)) in 
  {
    q0 = 0;
    f = StateSet.singleton 1;
    d;
  }

let seq n0 n1 = 
  let n0' = rn_even n0 in 
  let n1' = rn_odd n1 in
  {
  q0 = n0'.q0;
  f = n1'.f;
  d = fun s -> 
        let m = if s mod 2 = 0 then n0'.d s else n1'.d s in 
        if StateSet.mem s (n0'.f) then 
          CharOptMap.union merge m 
          (CharOptMap.singleton None (StateSet.singleton n1'.q0))
        else 
          m ;
} 

let kleene n = 
  let n' = rn_shift ~m:2 n in 
  {
    q0 = 0;
    f = StateSet.singleton 1;
    d = fun s -> 
          if s = 0 then 
            CharOptMap.singleton None (StateSet.of_list [n'.q0; 1])
          else
            (if s = 1 then 
              CharOptMap.singleton None (StateSet.singleton 0)
            else 
              let m = n'.d s in 
              (if StateSet.mem s n'.f then 
                CharOptMap.union 
                merge 
                (CharOptMap.singleton None (StateSet.of_list [n'.q0; 1]))
                m
               else 
                m)
            )
  }


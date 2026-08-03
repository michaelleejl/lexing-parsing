open Intfs 
open Ppx_compare_lib.Builtin

module Lookahead(Gram:Grammar.S) = struct 
  open Gram 

  module TSet = Set.Make(Gram.Terminal)

  module TerminalOption = struct 
    type t = Gram.terminal option 
      [@@deriving compare]
  end 
  module TOptSet = Set.Make(TerminalOption)

  module NTSet = Set.Make(Gram.Nonterminal)

  let unwrap topts = 
    TOptSet.fold (fun topt -> fun acc -> 
      match topt with  
      | Some v -> TSet.add v acc
      | None -> failwith "cannot unwrap None"
    ) topts TSet.empty 

  let rec fix_set f x = 
      let y = f x in 
      if NTSet.equal x y then x else fix_set f y

  module NTMap = Map.Make(Gram.Nonterminal)

  let rec fix_map eq f x = 
    let y = f x in 
    if NTMap.equal eq x y then x else fix_map eq f y 

  let strip_eps topts = TOptSet.diff topts (TOptSet.singleton None)

  let production_rules, consumption_rules =
    List.partition_map
      (function
        | Production { lhs; rhss } -> Left (lhs, rhss)
        | Consumption { lhs; action } -> Right (lhs, action))
      Gram.grammar

  let (nonterminals, productions) : nonterminal list * production list list =
    List.split production_rules

  let nt_to_prod_map = 
    List.fold_left (
      fun map -> fun (lhs, rhss) -> 
        NTMap.add lhs rhss map
    ) (NTMap.empty) production_rules

  let nullable_nonterms = 
    let is_sym_nullable nullable_nts = function
      | T _ -> false 
      | N n -> NTSet.mem n nullable_nts in 
    let is_rhs_nullable nullable_nts {rhs} = 
      List.for_all (is_sym_nullable nullable_nts) rhs in 
    let is_nullable nullable_nts nt = 
      NTMap.find nt nt_to_prod_map 
      |> List.exists (is_rhs_nullable nullable_nts) in
    let nullable_f nullable_nts = 
      List.filter (is_nullable nullable_nts) nonterminals
      |> NTSet.of_list  
    in fix_set nullable_f NTSet.empty 

  let nullable_sym = function 
    | T _ -> false 
    | N n-> NTSet.mem n nullable_nonterms

  let nullable_syms = List.for_all nullable_sym 


  let first_nonterms = 
    let delta firsts nonterminal {rhs} =
      let rec delta_syms = function 
      | [] -> TOptSet.empty 
      | T t :: _ -> TOptSet.singleton (Some t) 
      | N n :: syms -> 
          let ts = strip_eps (NTMap.find n firsts) in
          if nullable_sym (N n) then 
            TOptSet.union ts (delta_syms syms)
          else 
            ts in 
      let d = delta_syms rhs in 
      if (nullable_syms rhs) then 
        TOptSet.add None d
      else 
        d
     in
     let first_update_nt firsts (nonterminal, rhss) = 
      let first_set = NTMap.find nonterminal firsts in
      let deltas = List.map (delta firsts nonterminal) rhss
                |> List.fold_left (TOptSet.union) TOptSet.empty in 
      TOptSet.union first_set deltas in 

    let first_f firsts = 
      List.fold_left 
       (fun map -> fun ((nt, _) as p) -> 
        let new_firsts = first_update_nt firsts p in 
        NTMap.add nt new_firsts map 
        ) NTMap.empty production_rules in 
    let initial = List.fold_left 
      (fun map -> fun nt -> NTMap.add nt TOptSet.empty map) 
      NTMap.empty nonterminals in 
    fix_map (TOptSet.equal) first_f initial

  let first_sym = function 
    | T t -> TOptSet.singleton (Some t)
    | N n -> NTMap.find n first_nonterms

  let rec first_syms = function 
      | [] -> TOptSet.empty 
      | sym::syms -> 
          let first = first_sym sym in 
          if nullable_sym sym then 
            TOptSet.union (strip_eps first) (first_syms syms)
          else 
            first 
  let follow = 
    let union _ ts1 ts2 = Some(TSet.union ts1 ts2) in 
    let delta follows lhs {rhs} = 
      let rec delta_syms = function 
      | [] -> NTMap.empty 
      | T t::syms -> delta_syms syms 
      | N n::syms -> 

        let d_fs = 
          let first_sym = strip_eps (first_syms syms)
                          |> unwrap in 
          if nullable_syms syms then 
            TSet.union first_sym (NTMap.find lhs follows) 
          else
            first_sym
        in 
        let d = NTMap.singleton n d_fs in 
        NTMap.union union d (delta_syms syms)
      in delta_syms rhs 
    in 
    let deltas follows lhs rhss = 
      List.map (delta follows lhs) rhss 
      |> List.fold_left (NTMap.union union) (NTMap.empty)
    in
    let follow_f follows = 
      List.fold_left
      (fun acc -> fun (lhs, rhss) -> (
        NTMap.union union) acc (deltas follows lhs rhss))
      follows production_rules in
    let initial = 
      List.fold_left 
      (fun map -> fun nt -> NTMap.add nt TSet.empty map) 
      NTMap.empty nonterminals
    in 
    fix_map TSet.equal follow_f initial 
end 
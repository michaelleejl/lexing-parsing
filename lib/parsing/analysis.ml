open Intfs
open Ppx_compare_lib.Builtin

module Lookahead (Gram : Grammar.S) = struct
  open Gram
  module TSet = Set.Make (Gram.Terminal)

  module First = struct
    type t = Eps | Term of Gram.terminal [@@deriving compare]
  end

  module FirstSet = Set.Make (First)
  module NTSet = Set.Make (Gram.Nonterminal)

  let unwrap topts =
    FirstSet.fold
      (fun topt ->
        fun acc -> match topt with Term v -> TSet.add v acc | Eps -> acc)
      topts TSet.empty

  let rec fix eq f x =
    let y = f x in
    if eq x y then x else fix eq f y

  module NTMap = Map.Make (Gram.Nonterminal)

  let strip_eps topts = FirstSet.diff topts (FirstSet.singleton Eps)

  let production_rules, consumption_rules =
    List.partition_map
      (function
        | Production { lhs; rhss } -> Left (lhs, rhss)
        | Consumption { lhs; action } -> Right (lhs, action))
      Gram.grammar

  let (nonterminals, productions) : nonterminal list * production list list =
    List.split production_rules

  let nt_to_prod_map =
    List.fold_left
      (fun map -> fun (lhs, rhss) -> NTMap.add lhs rhss map)
      NTMap.empty production_rules

  let nullable_nonterms =
    let is_sym_nullable nullable_nts = function
      | T _ -> false
      | N n -> NTSet.mem n nullable_nts
    in
    let is_rhs_nullable nullable_nts { rhs } =
      List.for_all (is_sym_nullable nullable_nts) rhs
    in
    let is_nullable nullable_nts nt =
      NTMap.find nt nt_to_prod_map |> List.exists (is_rhs_nullable nullable_nts)
    in
    let nullable_step nullable_nts =
      List.filter (is_nullable nullable_nts) nonterminals |> NTSet.of_list
    in
    fix NTSet.equal nullable_step NTSet.empty

  let nullable_sym = function
    | T _ -> false
    | N n -> NTSet.mem n nullable_nonterms

  let nullable_syms = List.for_all nullable_sym

  let first_nonterms =
    let rec first_seq firsts = function
      (* computes the first set of a sequence *)
      | [] -> FirstSet.empty
      | T t :: _ -> FirstSet.singleton (Term t)
      | N n :: syms ->
          let ts = strip_eps (NTMap.find n firsts) in
          if nullable_sym (N n) then FirstSet.union ts (first_seq firsts syms)
          else ts
    in
    let first_rhs firsts { rhs } =
      (* computes the first set of RHS *)
      if nullable_syms rhs then FirstSet.add Eps (first_seq firsts rhs)
      else first_seq firsts rhs
    in
    let update_first firsts (nonterminal, rhss) =
      let first_set = NTMap.find nonterminal firsts in
      let updates =
        List.map (first_rhs firsts) rhss
        |> List.fold_left FirstSet.union FirstSet.empty
      in
      FirstSet.union first_set updates
    in

    let step firsts =
      List.fold_left
        (fun map ->
          fun ((nt, _) as p) ->
           let new_firsts = update_first firsts p in
           NTMap.add nt new_firsts map)
        firsts production_rules
    in
    let initial =
      List.fold_left
        (fun map -> fun nt -> NTMap.add nt FirstSet.empty map)
        NTMap.empty nonterminals
    in
    fix (NTMap.equal FirstSet.equal) step initial

  let first_sym = function
    | T t -> FirstSet.singleton (Term t)
    | N n -> NTMap.find n first_nonterms

  let rec first_syms = function
    | [] -> FirstSet.empty
    | sym :: syms ->
        let first = first_sym sym in
        if nullable_sym sym then
          FirstSet.union (strip_eps first) (first_syms syms)
        else first

  let follow_nonterms =
    let union_sets _ ts1 ts2 = Some (TSet.union ts1 ts2) in
    let follow_rhs follows lhs { rhs } =
      let rec follow_seq = function
        | [] -> NTMap.empty
        | T t :: syms -> follow_seq syms
        | N n :: syms ->
            let first_after = first_syms syms |> strip_eps |> unwrap in
            let contribution =
              if nullable_syms syms then
                TSet.union first_after (NTMap.find lhs follows)
              else first_after
            in
            NTMap.update n
              (function
                | None -> Some contribution
                | Some old -> Some (TSet.union old contribution))
              (follow_seq syms)
      in
      follow_seq rhs
    in
    let follow_updates follows lhs rhss =
      List.map (follow_rhs follows lhs) rhss
      |> List.fold_left (NTMap.union union_sets) NTMap.empty
    in
    let follow_step follows =
      List.fold_left
        (fun follow_map ->
          fun (lhs, rhss) ->
           (NTMap.union union_sets) follow_map (follow_updates follows lhs rhss))
        follows production_rules
    in
    let initial =
      List.fold_left
        (fun map -> fun nt -> NTMap.add nt TSet.empty map)
        NTMap.empty nonterminals
    in
    fix (NTMap.equal TSet.equal) follow_step initial
  
  let follow nonterm = NTMap.find nonterm follow_nonterms 
  
  end

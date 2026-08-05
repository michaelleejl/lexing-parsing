open Intfs
open Ppx_compare_lib.Builtin

module GrammarAnalysis (Gram : Grammar.S) = struct
  open Gram
  module TSet = Set.Make (Gram.Terminal)
  module NTMap = Map.Make (Gram.Nonterminal)
  module NTSet = Set.Make (Gram.Nonterminal)

  module TE = struct
    type t = Term of Gram.terminal | Eps [@@deriving compare]
  end

  open TE
  module TESet = Set.Make (TE)

  let unwrap te_set =
    TESet.fold
      (fun te acc -> match te with Eps -> acc | Term t -> TSet.add t acc)
      te_set TSet.empty

  let strip te_set = TESet.diff te_set (TESet.singleton Eps)

  let rec fix eq f x =
    let y = f x in
    if eq x y then x else fix eq f y

  let production_rules, consumption_rules =
    List.partition_map
      (function
        | Production { lhs; rhss } -> Left (lhs, rhss)
        | Consumption { lhs; action } -> Right (lhs, action))
      Gram.grammar

  let production_rules =
    (Gram.start, [ Gram.start_production ]) :: production_rules

  let (nonterminals, productions) : nonterminal list * production list list =
    List.split production_rules

  let nt_to_prod_map =
    List.fold_left
      (fun map -> fun (lhs, rhss) -> NTMap.add lhs rhss map)
      NTMap.empty production_rules

  module Nullable = struct
    let set =
      let is_sym_nullable nullable = function
        | T _ -> false
        | N n -> NTSet.mem n nullable
      in
      let is_rhs_nullable nullable { rhs } =
        List.for_all (is_sym_nullable nullable) rhs
      in
      let is_nullable nullable nt =
        NTMap.find nt nt_to_prod_map |> List.exists (is_rhs_nullable nullable)
      in
      let step nullable =
        List.filter (is_nullable nullable) nonterminals |> NTSet.of_list
      in
      fix NTSet.equal step NTSet.empty

    let sym = function T _ -> false | N n -> NTSet.mem n set
    let syms = List.for_all sym
  end

  module First = struct
    let table =
      let rec first_seq firsts = function
        (* computes the first set of a sequence *)
        | [] -> TESet.empty
        | T t :: _ -> TESet.singleton (Term t)
        | N n :: syms ->
            let ts = NTMap.find n firsts |> strip in
            if Nullable.sym (N n) then TESet.union ts (first_seq firsts syms)
            else ts
      in
      let first_rhs firsts { rhs } =
        (* computes the first set of RHS *)
        if Nullable.syms rhs then TESet.add Eps (first_seq firsts rhs)
        else first_seq firsts rhs
      in
      let update_first firsts (nonterminal, rhss) =
        let first_set = NTMap.find nonterminal firsts in
        let updates =
          List.map (first_rhs firsts) rhss
          |> List.fold_left TESet.union TESet.empty
        in
        TESet.union first_set updates
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
          (fun map -> fun nt -> NTMap.add nt TESet.empty map)
          NTMap.empty nonterminals
      in
      fix (NTMap.equal TESet.equal) step initial

    let sym = function
      | T t -> TESet.singleton (Term t)
      | N n -> NTMap.find n table

    let rec syms = function
      | [] -> TESet.empty
      | s :: ss ->
          let first_sym = sym s in
          if Nullable.sym s then TESet.union (strip first_sym) (syms ss)
          else first_sym
  end

  module Follow = struct
    let table =
      let union_sets _ ts1 ts2 = Some (TSet.union ts1 ts2) in
      let follow_rhs follows lhs { rhs } =
        let rec follow_seq = function
          | [] -> NTMap.empty
          | T t :: syms -> follow_seq syms
          | N n :: syms ->
              let first_after = First.syms syms |> strip |> unwrap in
              let contribution =
                if Nullable.syms syms then
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
             (NTMap.union union_sets) follow_map
               (follow_updates follows lhs rhss))
          follows production_rules
      in
      let initial =
        List.fold_left
          (fun map -> fun nt -> NTMap.add nt TSet.empty map)
          NTMap.empty nonterminals
      in
      fix (NTMap.equal TSet.equal) follow_step initial

    let nonterminal nonterm = NTMap.find nonterm table
  end
end

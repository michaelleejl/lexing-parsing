open Lang
open Fixpoint
open Ppx_compare_lib.Builtin

exception CyclicGrammar

module GrammarAnalysis (Bnf : AUGMENTED_BNF) = struct
  open Bnf
  open Views (Bnf)
  module TSet = Set.Make (Terminal)
  module NTMap = Map.Make (Nonterminal)
  module NTSet = Set.Make (Nonterminal)

  module TE = struct
    type t = Term of terminal | Eps [@@deriving compare]
  end

  open TE
  module TESet = Set.Make (TE)

  let to_terminals te_set =
    TESet.fold
      (fun te acc -> match te with Eps -> acc | Term t -> TSet.add t acc)
      te_set TSet.empty

  let strip_eps te_set = TESet.diff te_set (TESet.singleton Eps)

  module Nullable = struct
    let nonterminals =
      let is_sym_nullable nullable = function
        | T _ -> false
        | N n -> NTSet.mem n nullable
      in
      let is_rhs_nullable nullable { rhs } =
        List.for_all (is_sym_nullable nullable) rhs
      in
      let step nullable =
        List.filter_map
          (fun (p : production) ->
            if is_rhs_nullable nullable p then Some p.lhs else None)
          all_productions
        |> NTSet.of_list
      in
      fix ~eq:NTSet.equal step NTSet.empty

    let sym = function T _ -> false | N n -> NTSet.mem n nonterminals
    let syms = List.for_all sym
  end

  module First = struct
    let table =
      let rec first_seq firsts = function
        (* computes the first set of a sequence *)
        | [] -> TESet.empty
        | T t :: _ -> TESet.singleton (Term t)
        | N n :: syms ->
            let ts = NTMap.find n firsts |> strip_eps in
            if Nullable.sym (N n) then TESet.union ts (first_seq firsts syms)
            else ts
      in
      let first_rhs firsts { rhs } =
        (* computes the first set of RHS *)
        if Nullable.syms rhs then TESet.add Eps (first_seq firsts rhs)
        else first_seq firsts rhs
      in
      let step firsts =
        List.fold_left
          (fun map (p : production) ->
            NTMap.add p.lhs
              (TESet.union (NTMap.find p.lhs map) (first_rhs firsts p))
              map)
          firsts all_productions
      in
      let initial =
        List.fold_left
          (fun map (p : production) -> NTMap.add p.lhs TESet.empty map)
          NTMap.empty all_productions
      in
      fix ~eq:(NTMap.equal TESet.equal) step initial

    let sym = function
      | T t -> TESet.singleton (Term t)
      | N n -> NTMap.find n table

    let rec syms = function
      | [] -> TESet.singleton Eps
      | s :: ss ->
          let first_sym = sym s in
          if Nullable.sym s then TESet.union (strip_eps first_sym) (syms ss)
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
              let first_after = First.syms syms |> to_terminals in
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
      let follow_step follows =
        List.fold_left
          (fun follow_map (p : production) ->
            NTMap.union union_sets follow_map (follow_rhs follows p.lhs p))
          follows all_productions
      in
      let initial =
        List.fold_left
          (fun map (p : production) -> NTMap.add p.lhs TSet.empty map)
          (NTMap.singleton start (TSet.singleton eof_terminal))
          productions.rest
      in
      fix ~eq:(NTMap.equal TSet.equal) follow_step initial

    let nonterminal nonterm = NTMap.find nonterm table
  end

  module Cycles = struct
    let find nt map =
      match NTMap.find_opt nt map with Some x -> x | None -> NTSet.empty

    let table =
      let init map nt =
        let rec rhs_to_set acc = function
          | [] | T _ :: _ -> acc
          | N n :: syms ->
              let acc' = if Nullable.syms syms then NTSet.add n acc else acc in
              if Nullable.sym (N n) then rhs_to_set acc' syms else acc'
        in
        let productions = productions_of_nonterminal nt in
        let rhss = List.map (fun p -> p.rhs) productions in
        let nts = List.fold_left rhs_to_set NTSet.empty rhss in

        NTMap.add nt nts map
      in
      let update map =
        NTMap.map
          (fun set ->
            NTSet.fold (fun nt acc -> NTSet.union (find nt map) acc) set set)
          map
      in
      let initial = List.fold_left init NTMap.empty nonterminals in
      fix ~eq:(NTMap.equal NTSet.equal) update initial
    ;;

    NTMap.iter
      (fun nt set -> if NTSet.mem nt set then raise CyclicGrammar)
      table
  end
end

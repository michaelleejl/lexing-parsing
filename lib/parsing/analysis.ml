open Lang
open Ppx_compare_lib.Builtin

module GrammarAnalysis (Grammar : BNF) = struct
  open Grammar
  module TSet = Set.Make (Grammar.Terminal)
  module NTMap = Map.Make (Grammar.Nonterminal)
  module NTSet = Set.Make (Grammar.Nonterminal)

  module TE = struct
    type t = Term of Grammar.terminal | Eps [@@deriving compare]
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

  let all_productions = Grammar.start_production :: Grammar.productions

  module Nullable = struct
    let set =
      let is_sym_nullable nullable = function
        | T _ -> false
        | N n -> NTSet.mem n nullable
      in
      let is_rhs_nullable nullable { rhs } =
        List.for_all (is_sym_nullable nullable) rhs
      in
      let step nullable =
        List.filter_map
          (fun (p : p_action production) ->
            if is_rhs_nullable nullable p then Some p.lhs else None)
          all_productions
        |> NTSet.of_list
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
      let step firsts =
        List.fold_left
          (fun map (p : p_action production) ->
            NTMap.add p.lhs
              (TESet.union (NTMap.find p.lhs map) (first_rhs firsts p))
              map)
          firsts all_productions
      in
      let initial =
        List.fold_left
          (fun map (p : p_action production) -> NTMap.add p.lhs TESet.empty map)
          NTMap.empty all_productions
      in
      fix (NTMap.equal TESet.equal) step initial

    let sym = function
      | T t -> TESet.singleton (Term t)
      | N n -> NTMap.find n table

    let rec syms = function
      | [] -> TESet.singleton Eps
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
      let follow_step follows =
        List.fold_left
          (fun follow_map (p : p_action production) ->
            NTMap.union union_sets follow_map (follow_rhs follows p.lhs p))
          follows all_productions
      in
      let initial =
        List.fold_left
          (fun map (p : p_action production) -> NTMap.add p.lhs TSet.empty map)
          NTMap.empty all_productions
      in
      fix (NTMap.equal TSet.equal) follow_step initial

    let nonterminal nonterm = NTMap.find nonterm table
  end
end

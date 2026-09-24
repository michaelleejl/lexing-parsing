open Lang
open Fixpoint
open Ppx_compare_lib.Builtin

exception Cyclic_grammar

module Grammar_analysis (Bnf : AUGMENTED_BNF) = struct
  open Bnf
  open Views (Bnf)
  module Terminal_set = Set.Make (Terminal)
  module Nonterminal_map = Map.Make (Nonterminal)
  module Nonterminal_set = Set.Make (Nonterminal)

  module Term_or_eps = struct
    type t = Term of terminal | Eps [@@deriving compare]
  end

  open Term_or_eps
  module Term_or_eps_set = Set.Make (Term_or_eps)

  let to_terminals te_set =
    Term_or_eps_set.fold
      (fun te acc ->
        match te with Eps -> acc | Term t -> Terminal_set.add t acc)
      te_set Terminal_set.empty

  let strip_eps te_set =
    Term_or_eps_set.diff te_set (Term_or_eps_set.singleton Eps)

  module Nullable = struct
    let nonterminals =
      let is_sym_nullable nullable = function
        | T _ -> false
        | N n -> Nonterminal_set.mem n nullable
      in
      let is_rhs_nullable nullable { rhs } =
        List.for_all (is_sym_nullable nullable) rhs
      in
      let step nullable =
        List.filter_map
          (fun (p : production) ->
            if is_rhs_nullable nullable p then Some p.lhs else None)
          all_productions
        |> Nonterminal_set.of_list
      in
      fix ~eq:Nonterminal_set.equal step Nonterminal_set.empty

    let sym = function
      | T _ -> false
      | N n -> Nonterminal_set.mem n nonterminals

    let syms = List.for_all sym
  end

  module First = struct
    let table =
      let rec first_seq firsts = function
        (* computes the first set of a sequence *)
        | [] -> Term_or_eps_set.empty
        | T t :: _ -> Term_or_eps_set.singleton (Term t)
        | N n :: syms ->
            let ts = Nonterminal_map.find n firsts |> strip_eps in
            if Nullable.sym (N n) then
              Term_or_eps_set.union ts (first_seq firsts syms)
            else ts
      in
      let first_rhs firsts { rhs } =
        (* computes the first set of RHS *)
        if Nullable.syms rhs then Term_or_eps_set.add Eps (first_seq firsts rhs)
        else first_seq firsts rhs
      in
      let step firsts =
        List.fold_left
          (fun map (p : production) ->
            Nonterminal_map.add p.lhs
              (Term_or_eps_set.union
                 (Nonterminal_map.find p.lhs map)
                 (first_rhs firsts p))
              map)
          firsts all_productions
      in
      let initial =
        List.fold_left
          (fun map (p : production) ->
            Nonterminal_map.add p.lhs Term_or_eps_set.empty map)
          Nonterminal_map.empty all_productions
      in
      fix ~eq:(Nonterminal_map.equal Term_or_eps_set.equal) step initial

    let sym = function
      | T t -> Term_or_eps_set.singleton (Term t)
      | N n -> Nonterminal_map.find n table

    let rec syms = function
      | [] -> Term_or_eps_set.singleton Eps
      | s :: ss ->
          let first_sym = sym s in
          if Nullable.sym s then
            Term_or_eps_set.union (strip_eps first_sym) (syms ss)
          else first_sym
  end

  module Follow = struct
    let table =
      let union_sets _ ts1 ts2 = Some (Terminal_set.union ts1 ts2) in
      let follow_rhs follows lhs { rhs } =
        let rec follow_seq = function
          | [] -> Nonterminal_map.empty
          | T t :: syms -> follow_seq syms
          | N n :: syms ->
              let first_after = First.syms syms |> to_terminals in
              let contribution =
                if Nullable.syms syms then
                  Terminal_set.union first_after
                    (Nonterminal_map.find lhs follows)
                else first_after
              in
              Nonterminal_map.update n
                (function
                  | None -> Some contribution
                  | Some old -> Some (Terminal_set.union old contribution))
                (follow_seq syms)
        in
        follow_seq rhs
      in
      let follow_step follows =
        List.fold_left
          (fun follow_map (p : production) ->
            Nonterminal_map.union union_sets follow_map
              (follow_rhs follows p.lhs p))
          follows all_productions
      in
      let initial =
        List.fold_left
          (fun map (p : production) ->
            Nonterminal_map.add p.lhs Terminal_set.empty map)
          (Nonterminal_map.singleton start
             (Terminal_set.singleton eof_terminal))
          productions.rest
      in
      fix ~eq:(Nonterminal_map.equal Terminal_set.equal) follow_step initial

    let nonterminal nonterm = Nonterminal_map.find nonterm table
  end

  module Cycles = struct
    let find nt map =
      match Nonterminal_map.find_opt nt map with
      | Some x -> x
      | None -> Nonterminal_set.empty

    let table =
      let init map nt =
        let rec rhs_to_set acc = function
          | [] | T _ :: _ -> acc
          | N n :: syms ->
              let acc' =
                if Nullable.syms syms then Nonterminal_set.add n acc else acc
              in
              if Nullable.sym (N n) then rhs_to_set acc' syms else acc'
        in
        let productions = productions_of_nonterminal nt in
        let rhss = List.map (fun p -> p.rhs) productions in
        let nts = List.fold_left rhs_to_set Nonterminal_set.empty rhss in

        Nonterminal_map.add nt nts map
      in
      let update map =
        Nonterminal_map.map
          (fun set ->
            Nonterminal_set.fold
              (fun nt acc -> Nonterminal_set.union (find nt map) acc)
              set set)
          map
      in
      let initial = List.fold_left init Nonterminal_map.empty nonterminals in
      fix ~eq:(Nonterminal_map.equal Nonterminal_set.equal) update initial
    ;;

    Nonterminal_map.iter
      (fun nt set -> if Nonterminal_set.mem nt set then raise Cyclic_grammar)
      table
  end
end

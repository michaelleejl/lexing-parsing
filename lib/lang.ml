open Ppx_compare_lib.Builtin

module type VOCABULARY = sig
  type input
  type token
  type spec
  type action = input list -> token option

  val rules : (spec * action) list
end

(* User-facing Definitions *)

module type BNF = sig
  module Terminal : sig
    type t [@@deriving compare, to_string]
  end

  type terminal = Terminal.t [@@deriving compare, to_string]

  module Nonterminal : sig
    type t [@@deriving compare, to_string]
  end

  type nonterminal = Nonterminal.t [@@deriving compare, to_string]

  module Sym : sig
    type t = T of terminal | N of nonterminal [@@deriving compare, to_string]
  end

  type sym = Sym.t = T of terminal | N of nonterminal
  [@@deriving compare, to_string]

  type p_action
  type c_action
  type production = { lhs : nonterminal; rhs : sym list; action : p_action }
  type consumption = { lhs : terminal; action : c_action }

  val start_production : production
  val non_start_productions : production list
  val consumptions : consumption list
end

module type GRAMMAR = sig
  exception Fail

  type token
  type ast
  type data [@@deriving compare]
  type reduce
  type shift

  include BNF with type p_action = reduce and type c_action = shift

  val reduce : reduce -> data list -> data
  val shift : shift -> token -> data
  val unwrap : data -> ast
  val token_to_terminal : token -> terminal
  val eof : token
end

(* Compiler facing definitions*)

module type ELABORATED_BNF = sig
  module Terminal : sig
    type t [@@deriving compare, to_string]
  end

  type terminal = Terminal.t [@@deriving compare, to_string]

  module Nonterminal : sig
    type t [@@deriving compare, to_string]
  end

  type nonterminal = Nonterminal.t [@@deriving compare, to_string]

  module Sym : sig
    type t = T of terminal | N of nonterminal [@@deriving compare, to_string]
  end

  type sym = Sym.t = T of terminal | N of nonterminal
  [@@deriving compare, to_string]

  type production = { lhs : nonterminal; rhs : sym list } [@@deriving compare]
  type productions = { start : production; rest : production list }

  val productions : productions
  val eof_terminal : terminal
end

module type ELABORATED_GRAMMAR = sig
  exception Fail

  type token
  type ast
  type data [@@deriving compare]
  type reduce
  type shift

  module Bnf : ELABORATED_BNF

  val action : Bnf.production -> reduce
  val consume : Bnf.terminal -> shift
  val reduce : reduce -> data list -> data
  val shift : shift -> token -> data
  val unwrap : data -> ast
  val token_to_terminal : token -> Bnf.terminal
  val eof : token
end

exception Duplicate_production
exception Duplicate_consumption
exception Unconsumable_terminal of string

module Elaborate (Grammar : GRAMMAR) :
  ELABORATED_GRAMMAR
    with type token = Grammar.token
     and type ast = Grammar.ast
     and type data = Grammar.data
     and type reduce = Grammar.reduce
     and type shift = Grammar.shift
     and module Bnf.Terminal = Grammar.Terminal
     and module Bnf.Nonterminal = Grammar.Nonterminal
     and module Bnf.Sym = Grammar.Sym = struct
  exception Fail = Grammar.Fail

  type token = Grammar.token
  type ast = Grammar.ast
  type data = Grammar.data [@@deriving compare]
  type reduce = Grammar.reduce
  type shift = Grammar.shift

  module Bnf = struct
    module Terminal = Grammar.Terminal

    type terminal = Terminal.t [@@deriving compare, to_string]

    module Nonterminal = Grammar.Nonterminal

    type nonterminal = Nonterminal.t [@@deriving compare, to_string]

    module Sym = Grammar.Sym

    type sym = Sym.t =
      | T of terminal [@stringable.nested ""]
      | N of nonterminal [@stringable.nested ""]
    [@@deriving compare, to_string]

    type production = { lhs : nonterminal; rhs : sym list } [@@deriving compare]
    type productions = { start : production; rest : production list }

    let pure (p : Grammar.production) = { lhs = p.lhs; rhs = p.rhs }

    let productions =
      {
        start = pure Grammar.start_production;
        rest = List.map pure Grammar.non_start_productions;
      }

    let eof_terminal = Grammar.token_to_terminal Grammar.eof
  end

  module PMap = Map.Make (struct
    type t = Bnf.production

    let compare = Bnf.compare_production
  end)

  module TMap = Map.Make (Grammar.Terminal)

  let reduce_of_production =
    List.fold_left
      (fun map (p : Grammar.production) ->
        PMap.update (Bnf.pure p)
          (function
            | None -> Some p.action | Some _ -> raise Duplicate_production)
          map)
      PMap.empty
      (Grammar.start_production :: Grammar.non_start_productions)

  let shift_of_terminal =
    List.fold_left
      (fun map (c : Grammar.consumption) ->
        TMap.update c.lhs
          (function
            | None -> Some c.action | Some _ -> raise Duplicate_consumption)
          map)
      TMap.empty Grammar.consumptions

  let action p = PMap.find p reduce_of_production

  let consume t =
    match TMap.find_opt t shift_of_terminal with
    | Some s -> s
    | None -> raise (Unconsumable_terminal (Bnf.string_of_terminal t))

  let reduce = Grammar.reduce
  let shift = Grammar.shift
  let unwrap = Grammar.unwrap
  let token_to_terminal = Grammar.token_to_terminal
  let eof = Grammar.eof
end

module Views (Bnf : ELABORATED_BNF) = struct
  open Bnf
  module NTMap = Map.Make (Nonterminal)
  module TSet = Set.Make (Terminal)

  let all_productions = productions.start :: productions.rest
  let start = productions.start.lhs

  let productions_by_lhs =
    List.fold_right
      (fun (p : production) map ->
        NTMap.update p.lhs
          (function None -> Some [ p ] | Some ps -> Some (p :: ps))
          map)
      all_productions NTMap.empty

  let productions_of_nonterminal n = NTMap.find n productions_by_lhs
  let production_rules = NTMap.bindings productions_by_lhs
  let nonterminals = NTMap.bindings productions_by_lhs |> List.map fst

  let terminals =
    List.fold_left
      (fun acc (p : production) ->
        List.fold_left
          (fun acc -> function T t -> TSet.add t acc | N _ -> acc)
          acc p.rhs)
      (TSet.singleton eof_terminal)
      all_productions
    |> TSet.elements
end

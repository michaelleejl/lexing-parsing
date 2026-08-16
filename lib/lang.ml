open Ppx_compare_lib.Builtin

module type VOCABULARY = sig
  type input
  type token [@@deriving compare]
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

  type builder
  type reader
  type production = { lhs : nonterminal; rhs : sym list; builder : builder }
  type consumption = { lhs : terminal; reader : reader }

  val start : nonterminal
  val productions : production list
  val consumptions : consumption list
end

module type GRAMMAR = sig
  exception Fail

  type token [@@deriving compare]
  type ast [@@deriving compare]

  module Data : sig
    type t [@@deriving compare]
  end

  type data = Data.t [@@deriving compare]

  include BNF

  val build : builder -> data list -> data
  val read : reader -> token -> data
  val finish : data -> ast
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

  type token [@@deriving compare]
  type ast [@@deriving compare]

  module Data : sig
    type t [@@deriving compare]

    val start : t
  end

  type data [@@deriving compare]
  type builder
  type reader

  module Bnf : ELABORATED_BNF

  val builder_of_production : Bnf.production -> builder
  val reader_of_terminal : Bnf.terminal -> reader
  val build : builder -> data list -> data
  val read : reader -> token -> data
  val finish : data -> ast
  val token_to_terminal : token -> Bnf.terminal
  val eof : token
end

exception Duplicate_production
exception Duplicate_consumption
exception Unconsumable_terminal of string

module Augment_Nonterminals (Nonterminal : sig
  type t [@@deriving compare, to_string]
end) =
struct
  type t =
    | Start [@stringable.rename "S'"]
    | Source of Nonterminal.t [@stringable.nested ""]
  [@@deriving compare, to_string]
end

module Augment_Data (Data : sig
  type t [@@deriving compare]
end) =
struct
  type t = StartSym | SourceDatum of Data.t [@@deriving compare]

  exception UnwrapError

  let unwrap = function StartSym -> raise UnwrapError | SourceDatum d -> d
  let start = StartSym
end

module Elaborate (Grammar : GRAMMAR) :
  ELABORATED_GRAMMAR
    with type token = Grammar.token
     and type ast = Grammar.ast
     and type reader = Grammar.reader
     and module Bnf.Terminal = Grammar.Terminal
     and module Bnf.Nonterminal = Augment_Nonterminals(Grammar.Nonterminal)
     and module Data = Augment_Data(Grammar.Data) = struct
  exception Fail = Grammar.Fail

  type token = Grammar.token [@@deriving compare]
  type ast = Grammar.ast [@@deriving compare]

  module Data = Augment_Data (Grammar.Data)
  open Data

  type data = Data.t [@@deriving compare]
  type reader = Grammar.reader

  module Bnf = struct
    module Terminal = Grammar.Terminal

    type terminal = Terminal.t [@@deriving compare, to_string]

    module Nonterminal = Augment_Nonterminals (Grammar.Nonterminal)

    type nonterminal = Nonterminal.t [@@deriving compare, to_string]

    module Sym = struct
      type t =
        | T of terminal [@stringable.nested ""]
        | N of nonterminal [@stringable.nested ""]
      [@@deriving compare, to_string]
    end

    type sym = Sym.t =
      | T of terminal [@stringable.nested ""]
      | N of nonterminal [@stringable.nested ""]
    [@@deriving compare, to_string]

    type production = { lhs : nonterminal; rhs : sym list } [@@deriving compare]
    type productions = { start : production; rest : production list }

    let eof_terminal = Grammar.token_to_terminal Grammar.eof

    let lift : Grammar.sym -> sym = function
      | Grammar.T t -> T t
      | Grammar.N n -> N (Nonterminal.Source n)

    let pure (p : Grammar.production) =
      { lhs = Nonterminal.Source p.lhs; rhs = List.map lift p.rhs }

    let start =
      {
        lhs = Nonterminal.Start;
        rhs = [ N (Nonterminal.Source Grammar.start); T eof_terminal ];
      }

    let productions = { start; rest = List.map pure Grammar.productions }
  end

  module PMap = Map.Make (struct
    type t = Bnf.production

    let compare = Bnf.compare_production
  end)

  module TMap = Map.Make (Grammar.Terminal)

  type builder = Source of Grammar.builder | Start

  let build b ds =
    match b with
    | Source b -> SourceDatum (Grammar.build b (List.map Data.unwrap ds))
    | Start -> ( match ds with [ d; _ ] -> d | _ -> assert false)

  let builders =
    List.fold_left
      (fun map (p : Grammar.production) ->
        PMap.update (Bnf.pure p)
          (function
            | None -> Some (Source p.builder)
            | Some _ -> raise Duplicate_production)
          map)
      PMap.empty Grammar.productions
    |> PMap.add Bnf.start Start

  let readers =
    List.fold_left
      (fun map (r : Grammar.consumption) ->
        TMap.update r.lhs
          (function
            | None -> Some r.reader | Some _ -> raise Duplicate_consumption)
          map)
      TMap.empty Grammar.consumptions

  let builder_of_production p = PMap.find p builders

  let reader_of_terminal t =
    match TMap.find_opt t readers with
    | Some r -> r
    | None -> raise (Unconsumable_terminal (Bnf.string_of_terminal t))

  let read reader token = SourceDatum (Grammar.read reader token)
  let finish d = Grammar.finish (unwrap d)
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

  let symbols =
    List.map (fun x -> N x) nonterminals @ List.map (fun x -> T x) terminals
end

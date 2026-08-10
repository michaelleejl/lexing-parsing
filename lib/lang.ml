module type VOCABULARY = sig
  type input
  type token
  type spec
  type action = input list -> token option

  val rules : (spec * action) list
end

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

  type 'a production = {
    lhs : nonterminal ;
    rhs : sym list ;
    action : 'a
  }
  type 'a consumption = {
    lhs : terminal ;
    action : 'a
  }

  type p_action
  type c_action
  val start : nonterminal
  val start_production : p_action production
  val productions : p_action production list
  val consumptions : c_action consumption list 
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

module type ELABORATED_GRAMMAR = sig
  include GRAMMAR

  exception Duplicate_consumption

  type reduction_id = reduce [@@deriving compare]
  type shift_id = shift [@@deriving compare]

  val production_of_reduction_id : reduction_id -> p_action production option
  val productions_of : nonterminal -> p_action production list
  val shift_id_of_terminal : terminal -> shift_id option
  val eof_terminal : terminal
end

module Views (Bnf : BNF) = struct
  open Bnf
  module NTMap = Map.Make (Nonterminal)
  module TSet = Set.Make (Terminal)

  let production_rules =
    List.fold_right
      (fun (p : p_action production) map ->
        NTMap.update p.lhs
          (function None -> Some [ p ] | Some ps -> Some (p :: ps))
          map)
      (start_production :: productions)
      NTMap.empty
    |> NTMap.bindings

  let nonterminals = List.map fst production_rules
  let grouped_productions = List.map snd production_rules

  let terminals =
    List.map (fun (c : c_action consumption) -> c.lhs) consumptions
    |> TSet.of_list |> TSet.elements
end

module Elaborate (Grammar : GRAMMAR) :
  ELABORATED_GRAMMAR
    with type token = Grammar.token
     and type ast = Grammar.ast
     and type data = Grammar.data
     and module Terminal = Grammar.Terminal
     and module Nonterminal = Grammar.Nonterminal
     and module Sym = Grammar.Sym = struct
  type token = Grammar.token 
  type ast = Grammar.ast 
  module Terminal = Grammar.Terminal

  type terminal = Terminal.t [@@deriving compare, to_string]

  module Nonterminal = Grammar.Nonterminal
  type nonterminal = Nonterminal.t [@@deriving compare, to_string]

  module Sym = Grammar.Sym
  type sym = Sym.t =
    | T of terminal [@stringable.nested ""]
    | N of nonterminal [@stringable.nested ""]
  [@@deriving compare, to_string]

  type 'a production  = 'a Grammar.production  = { lhs : nonterminal ; rhs : sym list ; action : 'a }
  type 'a consumption = 'a Grammar.consumption = { lhs : terminal ; action : 'a }

  let start = Grammar.start
  type data = Grammar.data [@@deriving compare]

  exception Fail = Grammar.Fail
  exception Duplicate_consumption

  let unwrap = Grammar.unwrap

  module NTMap = Map.Make (Nonterminal)
  module TMap = Map.Make (Terminal)

  module ReductionRegistry =
    Registry.Make
      (struct
        type elt = Grammar.reduce
      end) 

  module ShiftRegistry = 
    Registry.Make 
      (struct 
        type elt = Grammar.shift
      end)

  type reduction_id = ReductionRegistry.Tag.t [@@deriving compare]
  type shift_id = ShiftRegistry.Tag.t [@@deriving compare]

  type reduce = reduction_id [@@deriving compare]
  type shift = shift_id [@@deriving compare]
  type p_action = reduce
  type c_action = shift

  module SMap = Map.Make(ShiftRegistry.Tag)
  module RMap = Map.Make(ReductionRegistry.Tag)

  let elaborate_production {lhs;rhs;action} =
    { lhs ; rhs ; action = ReductionRegistry.register action }

  let start_production = elaborate_production Grammar.start_production
  let productions = List.map elaborate_production Grammar.productions

  let reduce id ds = Grammar.reduce (ReductionRegistry.get id) ds
  
  let terminal_to_shift_id_map, shift_id_to_terminal_map = 
    List.fold_left 
    (fun (fwd_map, rev_map) {lhs;action} -> 
      let s_id = ShiftRegistry.register action in 
      let fwd = TMap.update lhs (function 
        | None -> Some s_id
        | Some _ -> raise Duplicate_consumption
      ) fwd_map in 
      let rev = SMap.update s_id (function 
        | None -> Some lhs
        | Some _ -> assert false 
      ) rev_map in 
      (fwd, rev)
    ) (TMap.empty, SMap.empty) Grammar.consumptions

  let shift_id_of_terminal t = TMap.find_opt t terminal_to_shift_id_map
  let shift id tok = Grammar.shift (ShiftRegistry.get id) tok

  let all_productions = start_production :: productions

  let reduction_id_to_production_map =
    List.fold_left
      (fun map (p : reduce production) ->
        RMap.update p.action
          (function None -> Some p | Some _ -> assert false)
          map)
      RMap.empty all_productions

  let production_of_reduction_id r_id =
    RMap.find_opt r_id reduction_id_to_production_map

  let productions_by_lhs =
    List.fold_right
      (fun (p : reduce production) map ->
        NTMap.update p.lhs
          (function None -> Some [ p ] | Some ps -> Some (p :: ps))
          map)
      all_productions NTMap.empty

  let productions_of n =
    NTMap.find_opt n productions_by_lhs |> Option.value ~default:[]

  let token_to_terminal = Grammar.token_to_terminal
  let eof = Grammar.eof
  let eof_terminal = token_to_terminal eof

  let consumptions =
    TMap.bindings terminal_to_shift_id_map
    |> List.map (fun (lhs, action) -> ({ lhs; action } : c_action consumption))
end

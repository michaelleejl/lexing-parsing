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

module GroupedProductions (Bnf : BNF) = struct 
  open Bnf 
  module NTMap = Map.Make(Nonterminal)

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
end

module type GRAMMAR = sig
  exception Fail

  type token
  type ast

  type data [@@deriving compare]
  type reduce = data list -> data
  type shift = token -> data

  include BNF with type p_action = reduce and type c_action = shift 

  val unwrap : data -> ast
  val token_to_terminal : token -> terminal
  val eof : token

end

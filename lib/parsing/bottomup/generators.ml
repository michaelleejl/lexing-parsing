open Lang
open Items
open States
open Tables
open Fixpoint
open Ppx_compare_lib.Builtin

exception ParseFail of string

module General (Grammar : GRAMMAR) = struct
  module Elaborated = BottomUp_Elaborate (Grammar)
  module Bnf = Elaborated.Bnf
  open Elaborated
  open Bnf
  open Views (Bnf)

  type token = Elaborated.token [@@deriving compare]
  type ast = Elaborated.ast [@@deriving compare]

  module Item = LR0.Make (Bnf)
  module State = States.Make (Item)
  open State
  module Actions = Actions (State)
  open Actions
  module Goto = Goto (State)

  type data_stack = data list [@@deriving compare]
  type state_stack = State.t list [@@deriving compare]

  type hypothesis = {
    data_stack : data_stack;
    state_stack : state_stack;
    tokens : token list;
  }
  [@@deriving compare]

  module ParseState = struct
    type t = Partial of hypothesis | Complete of ast [@@deriving compare]
  end

  type parse_state = ParseState.t = Partial of hypothesis | Complete of ast

  module ParseStates = Set.Make (ParseState)

  let accept args =
    Complete (finish (build (builder_of_production productions.start) args))

  let shift { data_stack; state_stack; tokens } =
    match tokens with
    | token :: tokens ->
        begin try
          let terminal = token_to_terminal token in
          let sym = T terminal in
          let datum = Elaborated.read (reader_of_terminal terminal) token in
          let data_stack = datum :: data_stack in
          let state = List.hd state_stack in
          let state_stack = next state sym :: state_stack in
          ParseStates.singleton (Partial { tokens; data_stack; state_stack })
        with Failure _ -> ParseStates.empty
        end
    | _ -> ParseStates.empty

  let reduce prod { data_stack; state_stack; tokens } =
    try
      let n = List.length prod.rhs in
      let args = List.take n data_stack |> List.rev in
      let datum = build (builder_of_production prod) args in
      let nonterminal = prod.lhs in
      let data_stack = datum :: List.drop n data_stack in
      let state_stack = List.drop n state_stack in
      let state = List.hd state_stack in
      let state_stack = Goto.find state nonterminal :: state_stack in
      ParseStates.singleton (Partial { state_stack; data_stack; tokens })
    with Not_found | Failure _ -> ParseStates.empty

  let interpret = function Shift -> shift | Reduce p -> reduce p

  let evolve = function
    | Complete a -> ParseStates.singleton (Complete a)
    | Partial ({ tokens; state_stack; data_stack } as hyp) -> (
        match (tokens, state_stack) with
        | [], _ -> raise (ParseFail "unexpected eof")
        | _, [] -> raise (ParseFail "unexpected empty state stack")
        | tok :: _, state :: state_stack ->
            let accept =
              if tok = eof && is_accepting state then
                ParseStates.singleton (accept data_stack)
              else ParseStates.empty
            in
            let terminal = token_to_terminal tok in
            let actions = Actions.find state terminal in
            List.fold_left
              (fun states act -> ParseStates.union (interpret act hyp) states)
              accept actions)

  let parse_step states =
    ParseStates.fold
      (fun state states -> ParseStates.union (evolve state) states)
      states ParseStates.empty

  let parse tokens =
    let initial =
      ParseStates.singleton
        (Partial
           {
             data_stack = [];
             state_stack = [ initial ];
             tokens = tokens @ [ eof ];
           })
    in
    fix ~eq:ParseStates.equal parse_step initial
    |> ParseStates.filter (function Complete _ -> true | Partial _ -> false)
    |> ParseStates.to_list
    |> function
    | [] -> raise (ParseFail "no valid hypotheses")
    | [ Complete a ] -> a
    | _ -> raise (ParseFail "ambiguous ast, multiple parses")
end

module SLR1 (Grammar : GRAMMAR) = struct
  module Elaborated = BottomUp_Elaborate (Grammar)
  module Bnf = Elaborated.Bnf
  open Elaborated
  open Bnf
  open Views (Bnf)

  type token = Elaborated.token [@@deriving compare]
  type ast = Elaborated.ast [@@deriving compare]

  module Item = LR0.Make (Bnf)
  module State = States.Make (Item)
  open State
  module Action = Action (State)
  open Action
  module Goto = Goto (State)

  type data_stack = data list [@@deriving compare]
  type state_stack = State.t list [@@deriving compare]

  type parse = {
    data_stack : data_stack;
    state_stack : state_stack;
    tokens : token list 
  }

  type parse_state = Partial of parse | Complete of ast 


  let accept args =
    Complete (finish (build (builder_of_production productions.start) args))

  let shift { data_stack; state_stack; tokens } =
    match tokens with
    | token :: tokens ->
        begin try
          let terminal = token_to_terminal token in
          let sym = T terminal in
          let datum = Elaborated.read (reader_of_terminal terminal) token in
          let data_stack = datum :: data_stack in
          let state = List.hd state_stack in
          let state_stack = next state sym :: state_stack in
          Partial { tokens; data_stack; state_stack }
        with Failure _ -> raise (ParseFail "malformed state stack")
        end
    | _ -> raise (ParseFail "malformed token stack")

  let reduce prod { data_stack; state_stack; tokens } =
    try
      let n = List.length prod.rhs in
      let args = List.take n data_stack |> List.rev in
      let datum = build (builder_of_production prod) args in
      let nonterminal = prod.lhs in
      let data_stack = datum :: List.drop n data_stack in
      let state_stack = List.drop n state_stack in
      let state = List.hd state_stack in
      let state_stack = Goto.find state nonterminal :: state_stack in
      Partial { state_stack; data_stack; tokens }
    with Failure _ -> raise (ParseFail "malformed parse state")

  let interpret = function Shift -> shift | Reduce p -> reduce p

  let step ({ tokens; state_stack; data_stack } as s) =
    match (tokens, state_stack) with
    | [], _ -> raise (ParseFail "unexpected eof")
    | _, [] -> raise (ParseFail "unexpected empty state stack")
    | tok :: _, state :: state_stack ->
        if tok = eof && is_accepting state then
            match data_stack with 
            | [d] -> accept data_stack
            | _ -> raise (ParseFail "malformed data stack")
          else 
        let terminal = token_to_terminal tok in
        let action = Action.find state terminal in
        interpret action s


  let rec run = function
    | Complete a -> a 
    | Partial s -> run (step s)

  let parse tokens = 
    let initial = Partial {
             data_stack = [];
             state_stack = [ initial ];
             tokens = tokens @ [ eof ];
           }
    in
    run initial
end

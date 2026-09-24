open Lang
open Items
open States
open Tables
open Fixpoint
open Ppx_compare_lib.Builtin

exception Parse_error of string

module Generalised (Grammar : GRAMMAR) = struct
  module Augmented = Bottomup_augment (Grammar)
  module Bnf = Augmented.Bnf
  open Augmented
  open Bnf
  open Views (Bnf)

  type token = Augmented.token [@@deriving compare]
  type ast = Augmented.ast [@@deriving compare]

  module Item = Lr0.Make (Bnf)
  module State = States.Make (Item)
  open State
  module Actions = Actions (State)
  open Actions
  module Goto = Goto (State)

  type data_stack = data list [@@deriving compare]
  type state_stack = State.t list [@@deriving compare]

  type config = {
    data_stack : data_stack;
    state_stack : state_stack;
    tokens : token list;
  }
  [@@deriving compare]

  module Outcome = struct
    type t = Partial of config | Accepted of ast [@@deriving compare]
  end

  type outcome = Outcome.t = Partial of config | Accepted of ast

  module Outcome_set = Set.Make (Outcome)

  let accept args =
    Accepted (finish (build (builder_of_production productions.start) args))

  let shift { data_stack; state_stack; tokens } =
    match tokens with
    | token :: tokens ->
        begin try
          let terminal = token_to_terminal token in
          let sym = T terminal in
          let datum = Augmented.read (reader_of_terminal terminal) token in
          let data_stack = datum :: data_stack in
          let state = List.hd state_stack in
          let state_stack = next state sym :: state_stack in
          Outcome_set.singleton (Partial { tokens; data_stack; state_stack })
        with Failure _ -> Outcome_set.empty
        end
    | _ -> Outcome_set.empty

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
      Outcome_set.singleton (Partial { state_stack; data_stack; tokens })
    with Not_found | Failure _ -> Outcome_set.empty

  let apply = function Shift -> shift | Reduce p -> reduce p

  let step = function
    | Accepted a -> Outcome_set.singleton (Accepted a)
    | Partial ({ tokens; state_stack; data_stack } as cfg) -> (
        match (tokens, state_stack) with
        | [], _ -> raise (Parse_error "unexpected eof")
        | _, [] -> raise (Parse_error "unexpected empty state stack")
        | tok :: _, state :: state_stack ->
            let accept =
              if tok = eof && is_accepting state then
                Outcome_set.singleton (accept data_stack)
              else Outcome_set.empty
            in
            let terminal = token_to_terminal tok in
            let actions = Actions.find state terminal in
            List.fold_left
              (fun states act -> Outcome_set.union (apply act cfg) states)
              accept actions)

  let step_all states =
    Outcome_set.fold
      (fun state states -> Outcome_set.union (step state) states)
      states Outcome_set.empty

  let parse tokens =
    let initial =
      Outcome_set.singleton
        (Partial
           {
             data_stack = [];
             state_stack = [ initial ];
             tokens = tokens @ [ eof ];
           })
    in
    fix ~eq:Outcome_set.equal step_all initial
    |> Outcome_set.filter (function Accepted _ -> true | Partial _ -> false)
    |> Outcome_set.to_list
    |> function
    | [] -> raise (Parse_error "no valid hypotheses")
    | [ Accepted a ] -> a
    | _ -> raise (Parse_error "ambiguous ast, multiple parses")
end

module Slr1 (Grammar : GRAMMAR) = struct
  module Augmented = Bottomup_augment (Grammar)
  module Bnf = Augmented.Bnf
  open Augmented
  open Bnf
  open Views (Bnf)

  type token = Augmented.token [@@deriving compare]
  type ast = Augmented.ast [@@deriving compare]

  module Item = Lr0.Make (Bnf)
  module State = States.Make (Item)
  open State
  module Action = Action (State)
  open Action
  module Goto = Goto (State)

  type data_stack = data list [@@deriving compare]
  type state_stack = State.t list [@@deriving compare]

  type config = {
    data_stack : data_stack;
    state_stack : state_stack;
    tokens : token list 
  }

  type outcome = Partial of config | Accepted of ast 


  let accept args =
    Accepted (finish (build (builder_of_production productions.start) args))

  let shift { data_stack; state_stack; tokens } =
    match tokens with
    | token :: tokens ->
        begin try
          let terminal = token_to_terminal token in
          let sym = T terminal in
          let datum = Augmented.read (reader_of_terminal terminal) token in
          let data_stack = datum :: data_stack in
          let state = List.hd state_stack in
          let state_stack = next state sym :: state_stack in
          Partial { tokens; data_stack; state_stack }
        with Failure _ -> raise (Parse_error "malformed state stack")
        end
    | _ -> raise (Parse_error "malformed token stack")

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
    with Failure _ -> raise (Parse_error "malformed parse state")

  let apply = function Shift -> shift | Reduce p -> reduce p

  let step ({ tokens; state_stack; data_stack } as s) =
    match (tokens, state_stack) with
    | [], _ -> raise (Parse_error "unexpected eof")
    | _, [] -> raise (Parse_error "unexpected empty state stack")
    | tok :: _, state :: state_stack ->
        if tok = eof && is_accepting state then
            match data_stack with 
            | [d] -> accept data_stack
            | _ -> raise (Parse_error "malformed data stack")
          else 
        let terminal = token_to_terminal tok in
        let action = Action.find state terminal in
        apply action s


  let rec run = function
    | Accepted a -> a 
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

module Lr1 (Grammar : GRAMMAR) = struct
  module Augmented = Bottomup_augment (Grammar)
  module Bnf = Augmented.Bnf
  open Augmented
  open Bnf
  open Views (Bnf)

  type token = Augmented.token [@@deriving compare]
  type ast = Augmented.ast [@@deriving compare]

  module Item = Lr1.Make (Bnf)
  module State = States.Make (Item)
  open State
  module Action = Action (State)
  open Action
  module Goto = Goto (State)

  type data_stack = data list [@@deriving compare]
  type state_stack = State.t list [@@deriving compare]

  type config = {
    data_stack : data_stack;
    state_stack : state_stack;
    tokens : token list 
  }

  type outcome = Partial of config | Accepted of ast 


  let accept args =
    Accepted (finish (build (builder_of_production productions.start) args))

  let shift { data_stack; state_stack; tokens } =
    match tokens with
    | token :: tokens ->
        begin try
          let terminal = token_to_terminal token in
          let sym = T terminal in
          let datum = Augmented.read (reader_of_terminal terminal) token in
          let data_stack = datum :: data_stack in
          let state = List.hd state_stack in
          let state_stack = next state sym :: state_stack in
          Partial { tokens; data_stack; state_stack }
        with Failure _ -> raise (Parse_error "malformed state stack")
        end
    | _ -> raise (Parse_error "malformed token stack")

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
    with Failure _ -> raise (Parse_error "malformed parse state")

  let apply = function Shift -> shift | Reduce p -> reduce p

  let step ({ tokens; state_stack; data_stack } as s) =
    match (tokens, state_stack) with
    | [], _ -> raise (Parse_error "unexpected eof")
    | _, [] -> raise (Parse_error "unexpected empty state stack")
    | tok :: _, state :: state_stack ->
        if tok = eof && is_accepting state then
            match data_stack with 
            | [d] -> accept data_stack
            | _ -> raise (Parse_error "malformed data stack")
          else 
        let terminal = token_to_terminal tok in
        let action = Action.find state terminal in
        apply action s


  let rec run = function
    | Accepted a -> a 
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

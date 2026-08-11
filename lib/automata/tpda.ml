open Params
open Ppx_compare_lib.Builtin

module Make (Input : INPUT) (StackSym : STACK_SYM) (Tag : TAG) = struct
  type input = Input.t
  type stack_sym = StackSym.t
  type tag = Tag.t

  module State = Set.Make (Int)

  type state_set = State.t
  type state = State.elt

  let compare_state = Int.compare

  module TransitionInput = struct
    type t = input option * stack_sym

    let compare = compare
  end

  module TransitionOutput = struct
    type t = state * stack_sym list * tag

    let compare = compare
  end

  module TransitionOutputSet = Set.Make (TransitionOutput)

  type transition_output_set = TransitionOutputSet.t

  module Transition = Map.Make (TransitionInput)

  type transition = transition_output_set Transition.t

  module Config = struct
    type t = { current_state : state; stack : StackSym.t list }
    [@@deriving compare]
  end

  type config = Config.t [@@deriving compare]

  module Trace = struct
    type t = Config.t * Tag.t list [@@deriving compare]
  end

  type trace = Trace.t

  module TraceSet = Set.Make (Trace)

  type t = {
    states : state_set;
    next : state -> transition;
    initial_state : state;
    initial_stack_sym : stack_sym;
  }

  exception StackEmptyException

  let is_accepting Config.{ stack } = List.is_empty stack
  let pop = function [] -> raise StackEmptyException | _ :: xs -> xs
  let peek = function [] -> raise StackEmptyException | x :: _ -> x

  let find pda state stack input =
    try
      let sym = peek stack in
      Transition.find (input, sym) (pda.next state)
    with
    | Not_found -> TransitionOutputSet.empty
    | StackEmptyException -> TransitionOutputSet.empty

  let rec step pda traces input =
    let f (({ current_state; stack }, tags) : trace) traces =
      let outputs = find pda current_state stack input in
      TraceSet.union traces (update_trace stack tags outputs)
    in
    TraceSet.fold f traces TraceSet.empty

  and update_trace stack tags outputs =
    let g (next_state, to_push, tag) traces =
      let new_cfg : config =
        { current_state = next_state; stack = to_push @ pop stack }
      in
      TraceSet.add (new_cfg, tag :: tags) traces
    in
    TransitionOutputSet.fold g outputs TraceSet.empty

  let step_eps pda cfgs = step pda cfgs None

  let epsilon_closure pda =
    Fixpoint.fix ~eq:TraceSet.equal (fun traces ->
        TraceSet.union traces (step_eps pda traces))

  let consume pda traces tok =
    let traces' = epsilon_closure pda traces in
    let traces'' = step pda traces' (Some tok) in
    let traces''' = epsilon_closure pda traces'' in
    TraceSet.map (fun (cfg, tr) -> (cfg, List.rev tr)) traces'''
end

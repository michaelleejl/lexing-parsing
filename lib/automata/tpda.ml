open Params
open Ppx_compare_lib.Builtin

module Make (Input : INPUT) (Stack_sym : STACK_SYM) (Tag : TAG) = struct
  type input = Input.t
  type stack_sym = Stack_sym.t
  type tag = Tag.t

  module State_set = Set.Make (Int)

  type state_set = State_set.t
  type state = State_set.elt

  let compare_state = Int.compare

  module Transition_input = struct
    type t = input option * stack_sym

    let compare = compare
  end

  module Transition_output = struct
    type t = state * stack_sym list * tag

    let compare = compare
  end

  module Transition_output_set = Set.Make (Transition_output)

  type transition_output_set = Transition_output_set.t

  module Transition_map = Map.Make (Transition_input)

  type transition = transition_output_set Transition_map.t

  module Config = struct
    type t = { current_state : state; stack : Stack_sym.t list }
    [@@deriving compare]
  end

  type config = Config.t [@@deriving compare]

  module Trace = struct
    type t = Config.t * Tag.t list [@@deriving compare]
  end

  type trace = Trace.t

  module Trace_set = Set.Make (Trace)

  type t = {
    states : state_set;
    next : state -> transition;
    initial_state : state;
    initial_stack_sym : stack_sym;
  }

  exception Empty_stack

  let is_accepting Config.{ stack } = List.is_empty stack
  let pop = function [] -> raise Empty_stack | _ :: xs -> xs
  let peek = function [] -> raise Empty_stack | x :: _ -> x

  let find pda state stack input =
    try
      let sym = peek stack in
      Transition_map.find (input, sym) (pda.next state)
    with
    | Not_found -> Transition_output_set.empty
    | Empty_stack -> Transition_output_set.empty

  let rec step pda traces input =
    let f (({ current_state; stack }, tags) : trace) traces =
      let outputs = find pda current_state stack input in
      Trace_set.union traces (update_trace stack tags outputs)
    in
    Trace_set.fold f traces Trace_set.empty

  and update_trace stack tags outputs =
    let g (next_state, to_push, tag) traces =
      let new_cfg : config =
        { current_state = next_state; stack = to_push @ pop stack }
      in
      Trace_set.add (new_cfg, tag :: tags) traces
    in
    Transition_output_set.fold g outputs Trace_set.empty

  let step_eps pda cfgs = step pda cfgs None

  let epsilon_closure pda =
    Fixpoint.fix ~eq:Trace_set.equal (fun traces ->
        Trace_set.union traces (step_eps pda traces))

  let consume pda traces tok =
    let traces' = epsilon_closure pda traces in
    let traces'' = step pda traces' (Some tok) in
    let traces''' = epsilon_closure pda traces'' in
    Trace_set.map (fun (cfg, tr) -> (cfg, List.rev tr)) traces'''
end

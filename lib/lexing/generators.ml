open Regex

let rec compile_regex r =
  match r with
  | Empty -> Nfa.empty
  | Epsilon -> Nfa.epsilon
  | Char cs -> Nfa.one_of (C.to_list cs)
  | Alt (r1, r2) -> Nfa.alt (compile_regex r1) (compile_regex r2)
  | Seq (r1, r2) -> Nfa.seq (compile_regex r1) (compile_regex r2)
  | Kleene r -> Nfa.kleene (compile_regex r)

module Recogniser = struct
  include Regex

  type s = Dfa.t

  let compile r = compile_regex r |> Dfa.determinise
  let recognise = Dfa.accept
end

open Intfs
open Intfs.Language
open Intfs.Tags

module Lexer (Lang : L) (Tag : T with type token = Lang.token) = struct
  exception LexFailure of string

  type token = Lang.token
  type m = Regex.t

  module TaggedDfa = Tdfa.Make (Tag)
  module TaggedNfa = TaggedDfa.TaggedNfa

  type tag_t = Tag.t
  type s = TaggedNfa.t
  type t = TaggedDfa.t

  module Matcher = struct
    include Regex
  end

  open TaggedDfa

  let tag matcher t = TaggedNfa.lift (compile_regex matcher) t
  let ( >>| ) = TaggedNfa.alt
  let compile = determinise

  type lexing_state = {
    state : state;
    rest : char list;
    tokens : token list;
    buffer : char list;
    last_accepting : (int * state) option;
  }

  let lex_step machine { state; rest; tokens; buffer; last_accepting } =
    if is_rejecting machine state then
      match last_accepting with
      | None -> raise (LexFailure "no last accepting state")
      | Some (k, qs) -> (
          let tag = emit_tag machine qs in
          match tag with
          | None -> raise (LexFailure "tag is empty")
          | Some tag -> (
              let chars = List.drop k buffer in
              let buffer = List.take k buffer in
              let action = (Tag.tag_to_action tag) (List.rev chars) in
              let last_accepting = None in
              let state = initialise machine in
              let rest = List.rev buffer @ rest in
              let buffer = [] in
              match action with
              | None -> { rest; tokens; buffer; last_accepting; state }
              | Some t ->
                  { rest; tokens = t :: tokens; buffer; last_accepting; state })
          )
    else
      match rest with
      | [] ->
          if is_accepting machine state then
            let tag = emit_tag machine state in
            match tag with
            | None -> raise (LexFailure "tag is empty")
            | Some tag -> (
                let action = (Tag.tag_to_action tag) (List.rev buffer) in
                match action with
                | None ->
                    { rest; tokens; buffer = []; last_accepting = None; state }
                | Some t ->
                    {
                      rest;
                      tokens = t :: tokens;
                      buffer = [];
                      last_accepting = None;
                      state;
                    })
          else raise (LexFailure "last state was not accepting")
      | c :: rest ->
          let next_state = step machine state c in
          let new_accepting =
            if is_accepting machine next_state then Some (0, next_state)
            else
              match last_accepting with
              | None -> None
              | Some (i, qs) -> Some (i + 1, qs)
          in
          {
            rest;
            tokens;
            buffer = c :: buffer;
            last_accepting = new_accepting;
            state = next_state;
          }

  let rec lex_run machine state =
    match (state.rest, state.buffer) with
    | [], [] -> List.rev state.tokens
    | _, _ -> lex_run machine (lex_step machine state)

  let lex machine s =
    let cs = Base.String.to_list s in
    let initial_state =
      {
        state = initialise machine;
        rest = cs;
        tokens = [];
        buffer = [];
        last_accepting = None;
      }
    in
    lex_run machine initial_state
end

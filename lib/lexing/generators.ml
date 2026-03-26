open Regex

module Recogniser = struct
  type r = Regex.t
  type t = Dfa.t

  let compile r = compile_regex r |> Dfa.determinise
  let recognise = Dfa.accept
end

open Intfs
open Intfs.Language
open Intfs.Tags

module Lexer (Lang : L) (Tag : T with type token = Lang.token) = struct
  module TaggedDfa = Tdfa.Make (Tag)
  module TaggedNfa = TaggedDfa.TaggedNfa

  type token = Lang.token
  type tag = Tag.t
  type r = Regex.t
  type s = TaggedNfa.t
  type t = TaggedDfa.t

  exception LexFailure of string

  open TaggedDfa

  let compile matcher t = TaggedNfa.lift (compile_regex matcher) t
  let ( >>| ) = TaggedNfa.alt
  let determinise = determinise

  type lexing_state = {
    state : state;
    rest : char list;
    tokens : token list;
    buffer : char list;
    last_accepting : (int * state) option;
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

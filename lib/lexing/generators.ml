open Regex

module RegexToNfa (N: Nfa.S with type input = char) = struct 
  let rec compile r =
  match r with
  | Empty -> N.empty
  | Epsilon -> N.epsilon
  | Char cs -> N.one_of (C.to_list cs)
  | Alt (r1, r2) -> N.alt (compile r1) (compile r2)
  | Seq (r1, r2) -> N.seq (compile r1) (compile r2)
  | Kleene r -> N.kleene (compile r)
end 

module Recogniser = struct
  module Dfa = Dfa.Make(Char)
  open Dfa 
  type r = Regex.t
  type t = Dfa.t

  module RegexCompiler = RegexToNfa(Nfa)
  let compile r = RegexCompiler.compile r |> Dfa.determinise
  let recognise dfa s =  Base.String.to_list s |> Dfa.accept dfa
end

open Intfs

module Lexer (Lang : Language.S) (Tag : Tags.S with type token = Lang.token and type input = char) =
struct
  module TaggedDfa = Tdfa.Make(Char)(Tag)
  module TaggedNfa = TaggedDfa.TaggedNfa

  type token = Lang.token
  type tag = Tag.t
  type r = Regex.t
  type s = TaggedNfa.t
  type t = TaggedDfa.t

  exception LexFailure of string

  open TaggedDfa

  module RegexCompiler = RegexToNfa(TaggedNfa.Nfa)

  let compile matcher t = TaggedNfa.lift (RegexCompiler.compile matcher) t
  let ( >>| ) = TaggedNfa.alt
  let determinise = determinise

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

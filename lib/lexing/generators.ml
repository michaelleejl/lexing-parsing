open Regex
open Automata.Nfa

module Regex_to_nfa (N : S with type input = char) = struct
  open N

  let rec compile r =
    match r with
    | Empty -> empty
    | Epsilon -> epsilon
    | Chars cs -> one_of (Charset.to_list cs)
    | Alt (r1, r2) -> alt (compile r1) (compile r2)
    | Seq (r1, r2) -> seq (compile r1) (compile r2)
    | Kleene r -> kleene (compile r)
end

module Recogniser = struct
  module Dfa = Automata.Dfa.Make (Char)
  open Dfa

  type r = Regex.t
  type t = Dfa.t

  module Regex_compiler = Regex_to_nfa (Nfa)

  let compile r = Regex_compiler.compile r |> Dfa.determinise
  let recognise dfa s = Base.String.to_list s |> Dfa.accept dfa
end

open Lang

module Lexer
    (Spec : LEXICAL_SPEC with type input = char and type spec = Charset.t regex) =
struct
  type token = Spec.token
  type action = char list -> token option

  module Action_registry = Registry.Make (struct
    type elt = action
  end)

  module Tagged_dfa = Automata.Tdfa.Make (Char) (Action_registry.Id)
  module Tagged_nfa = Tagged_dfa.Tagged_nfa
  module Nfa = Tagged_nfa.Nfa

  type tag = Action_registry.Id.t
  type r = Regex.t
  type s = Tagged_nfa.t
  type t = Tagged_dfa.t

  exception Lex_error of string

  open Tagged_dfa
  module Regex_compiler = Regex_to_nfa (Tagged_nfa.Nfa)

  let compile matcher action =
    let tag = Action_registry.register action in
    Tagged_nfa.lift (Regex_compiler.compile matcher) tag

  let ( <|> ) = Tagged_nfa.alt
  let determinise = determinise

  type lex_state = {
    state : state;
    rest : char list;
    tokens : token list;
    buffer : char list;
    last_accepting : (int * state) option;
  }

  let rec lex_step machine { state; rest; tokens; buffer; last_accepting } =
    if is_rejecting machine state then
      rollback machine state rest tokens buffer last_accepting
    else
      match rest with
      | [] ->
          if is_accepting machine state then
            emit machine tokens rest buffer 0 state
          else rollback machine state rest tokens buffer last_accepting
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

  and rollback machine state rest tokens buffer last_accepting =
    match last_accepting with
    | None -> raise (Lex_error "no last accepting state")
    | Some (k, qs) -> emit machine tokens rest buffer k qs

  and emit machine tokens rest buffer k qs =
    let tag = emit_tag machine qs in
    match tag with
    | None -> raise (Lex_error "tag is empty")
    | Some tag -> (
        let chars = List.drop k buffer in
        let buffer = List.take k buffer in
        let action = Action_registry.get tag (List.rev chars) in
        let last_accepting = None in
        let state = initialise machine in
        let rest = List.rev buffer @ rest in
        let buffer = [] in
        match action with
        | None -> { rest; tokens; buffer; last_accepting; state }
        | Some t ->
            { rest; tokens = t :: tokens; buffer; last_accepting; state })

  let rec lex_run machine state =
    match (state.rest, state.buffer) with
    | [], [] -> List.rev state.tokens
    | _, _ -> lex_run machine (lex_step machine state)

  let lexers = List.map (fun (r, a) -> compile r a) Spec.rules

  let empty_lexer =
    compile Regex.empty (fun _ -> raise (Lex_error "empty lexer"))

  let lexer = List.fold_right ( <|> ) lexers empty_lexer |> determinise

  let lex s =
    let cs = Base.String.to_list s in
    let initial_state =
      {
        state = initialise lexer;
        rest = cs;
        tokens = [];
        buffer = [];
        last_accepting = None;
      }
    in
    lex_run lexer initial_state
end

open Intfs
open Intfs.Language
open Regex

module Recogniser = struct
  type r = Regex.t
  type t = char list -> char list outcome

end

module Lexer (Lang : L) = struct
  type token = Lang.token
  type action = char list -> token option
  type r = Regex.t
  type matcher_state = { matched : char list; rest : char list }
  type s = char list -> matcher_state outcome
  type lex_state = { lexed : token list; rest : char list }
  type t = lex_state -> lex_state outcome

  exception LexFailure

  let lex_step l state =
    match l state with
    | Success { lexed; rest } -> { lexed; rest }
    | Failure -> raise LexFailure

  let rec lex_run l state =
    match lex_step l state with
    | { lexed; rest = [] } -> List.rev lexed
    | { lexed; rest } as state -> lex_run l state

  let lex l s =
    let cs = Base.String.to_list s in
    lex_run l { lexed = []; rest = cs }
end

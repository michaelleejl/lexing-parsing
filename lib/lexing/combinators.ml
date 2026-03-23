open Intfs
open Intfs.Language
open Regex

module Recogniser = struct
  type r = Regex.t
  type t = char list -> char list outcome

  let one_of cs = function
    | [] -> Failure
    | x :: xs -> if C.mem x cs then Success xs else Failure

  let emp _ = Failure
  let eps cs = Success cs
  let seq r1 r2 cs = match r1 cs with Failure -> Failure | Success cs -> r2 cs

  let alt r1 r2 cs =
    match (r1 cs, r2 cs) with
    | Failure, Failure -> Failure
    | Success xs, Failure -> Success xs
    | Failure, Success ys -> Success ys
    | Success xs, Success ys ->
        if List.length xs <= List.length ys then Success xs else Success ys

  let rec kleene r cs =
    match r cs with Failure -> Success cs | Success xs -> kleene r xs

  let rec interpret r =
    match r with
    | Empty -> emp
    | Epsilon -> eps
    | Char cs -> one_of cs
    | Alt (r1, r2) -> alt (interpret r1) (interpret r2)
    | Seq (r1, r2) -> seq (interpret r1) (interpret r2)
    | Kleene r -> kleene (interpret r)

  let recognise r s =
    match r (Base.String.to_list s) with Success [] -> true | _ -> false
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

  let one_of cs = function
    | [] -> Failure
    | x :: xs ->
        if Regex.C.mem x cs then Success { matched = [ x ]; rest = xs }
        else Failure

  let eps s = Success { matched = []; rest = s }
  let emp _ = Failure

  let seq m1 m2 cs =
    match m1 cs with
    | Failure -> Failure
    | Success { matched = matched1; rest } -> (
        match m2 rest with
        | Failure -> Failure
        | Success { matched = matched2; rest } ->
            Success { matched = matched1 @ matched2; rest })

  let alt (m1 : s) m2 cs =
    match (m1 cs, m2 cs) with
    | Failure, Failure -> Failure
    | Success s, Failure -> Success s
    | Failure, Success s' -> Success s'
    | Success s, Success s' ->
        if List.length s.rest <= List.length s'.rest then Success s
        else Success s'

  let kleene m cs =
    let rec kleene' m cs =
      match m cs with
      | Failure -> { matched = []; rest = cs }
      | Success { matched; rest } ->
          let { matched = matched'; rest = rest' } = kleene' m rest in
          { matched = matched @ matched'; rest = rest' }
    in
    Success (kleene' m cs)

  let rec interpret' r =
    match r with
    | Empty -> emp
    | Epsilon -> eps
    | Char cs -> one_of cs
    | Alt (r1, r2) -> alt (interpret' r1) (interpret' r2)
    | Seq (r1, r2) -> seq (interpret' r1) (interpret' r2)
    | Kleene r -> kleene (interpret' r)

  let interpret r to_token { lexed; rest } =
    let m = interpret' r in
    match m rest with
    | Failure -> Failure
    | Success { matched; rest } -> (
        match to_token matched with
        | None -> Success { lexed; rest }
        | Some t -> Success { lexed = t :: lexed; rest })

  let alt_l l1 l2 s =
    match (l1 s, l2 s) with
    | Failure, Failure -> Failure
    | Success s, Failure -> Success s
    | Failure, Success s' -> Success s'
    | Success s, Success s' ->
        if List.length s.rest <= List.length s'.rest then Success s
        else Success s'

  let ( >>| ) = alt_l

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

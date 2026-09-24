open Lang
open Regex

type 'a outcome = Matched of 'a | Unmatched

module Recogniser = struct
  type r = Regex.t
  type t = char list -> char list outcome

  let one_of cs = function
    | [] -> Unmatched
    | x :: xs -> if Charset.mem x cs then Matched xs else Unmatched

  let emp _ = Unmatched
  let eps cs = Matched cs
  let seq r1 r2 cs = match r1 cs with Unmatched -> Unmatched | Matched cs -> r2 cs

  let alt r1 r2 cs =
    match (r1 cs, r2 cs) with
    | Unmatched, Unmatched -> Unmatched
    | Matched xs, Unmatched -> Matched xs
    | Unmatched, Matched ys -> Matched ys
    | Matched xs, Matched ys ->
        if List.length xs <= List.length ys then Matched xs else Matched ys

  let rec kleene r cs =
    match r cs with Unmatched -> Matched cs | Matched xs -> kleene r xs

  let rec interpret r =
    match r with
    | Empty -> emp
    | Epsilon -> eps
    | Chars cs -> one_of cs
    | Alt (r1, r2) -> alt (interpret r1) (interpret r2)
    | Seq (r1, r2) -> seq (interpret r1) (interpret r2)
    | Kleene r -> kleene (interpret r)

  let recognise r s =
    match r (Base.String.to_list s) with Matched [] -> true | _ -> false
end

module Lexer
    (Spec : LEXICAL_SPEC with type input = char and type spec = Charset.t regex) =
struct
  type token = Spec.token
  type action = char list -> token option
  type r = Regex.t
  type matcher_state = { matched : char list; rest : char list }
  type s = char list -> matcher_state outcome
  type lex_state = { lexed : token list; rest : char list }
  type t = lex_state -> lex_state outcome

  exception Lex_error

  let one_of cs = function
    | [] -> Unmatched
    | x :: xs ->
        if Regex.Charset.mem x cs then Matched { matched = [ x ]; rest = xs }
        else Unmatched

  let eps s = Matched { matched = []; rest = s }
  let emp _ = Unmatched

  let seq m1 m2 cs =
    match m1 cs with
    | Unmatched -> Unmatched
    | Matched { matched = matched1; rest } -> (
        match m2 rest with
        | Unmatched -> Unmatched
        | Matched { matched = matched2; rest } ->
            Matched { matched = matched1 @ matched2; rest })

  let alt (m1 : s) m2 cs =
    match (m1 cs, m2 cs) with
    | Unmatched, Unmatched -> Unmatched
    | Matched s, Unmatched -> Matched s
    | Unmatched, Matched s' -> Matched s'
    | Matched s, Matched s' ->
        if List.length s.rest <= List.length s'.rest then Matched s
        else Matched s'

  let kleene m cs =
    let rec kleene' m cs =
      match m cs with
      | Unmatched -> { matched = []; rest = cs }
      | Matched { matched; rest } ->
          let { matched = matched'; rest = rest' } = kleene' m rest in
          { matched = matched @ matched'; rest = rest' }
    in
    Matched (kleene' m cs)

  let rec interpret' r =
    match r with
    | Empty -> emp
    | Epsilon -> eps
    | Chars cs -> one_of cs
    | Alt (r1, r2) -> alt (interpret' r1) (interpret' r2)
    | Seq (r1, r2) -> seq (interpret' r1) (interpret' r2)
    | Kleene r -> kleene (interpret' r)

  let interpret r to_token { lexed; rest } =
    let m = interpret' r in
    match m rest with
    | Unmatched -> Unmatched
    | Matched { matched; rest } -> (
        match to_token matched with
        | None -> Matched { lexed; rest }
        | Some t -> Matched { lexed = t :: lexed; rest })

  let alt_l l1 l2 s =
    match (l1 s, l2 s) with
    | Unmatched, Unmatched -> Unmatched
    | Matched s, Unmatched -> Matched s
    | Unmatched, Matched s' -> Matched s'
    | Matched s, Matched s' ->
        if List.length s.rest <= List.length s'.rest then Matched s
        else Matched s'

  let ( <|> ) = alt_l

  let lex_step l state =
    match l state with
    | Matched { lexed; rest } -> { lexed; rest }
    | Unmatched -> raise Lex_error

  let rec lex_run l state =
    match state with
    | { lexed; rest = [] } -> List.rev lexed
    | { lexed; rest } as state -> lex_run l (lex_step l state)

  let lexers = List.map (fun (r, a) -> interpret r a) Spec.rules
  let empty_lexer = interpret Regex.empty (fun _ -> raise Lex_error)
  let lexer = List.fold_right ( <|> ) lexers empty_lexer

  let lex s =
    let cs = Base.String.to_list s in
    lex_run lexer { lexed = []; rest = cs }
end

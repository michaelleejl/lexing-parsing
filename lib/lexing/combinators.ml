open Intfs
open Intfs.Language
open Regex

module Recogniser = struct
  include Regex

  type s = char list -> char list outcome

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
  open Base

  module Matcher = struct
    type matcher_state = { matched : char list; rest : char list }
    type matcher = char list -> matcher_state outcome

    let char_matcher f cs =
      match cs with
      | [] -> Failure
      | c :: cs ->
          if f c then Success { matched = [ c ]; rest = cs } else Failure

    let alphabetic = char_matcher Char.is_alpha
    let numeric = char_matcher Char.is_digit
    let alphanumeric = char_matcher Char.is_alphanum
    let whitespace = char_matcher Char.is_whitespace
    let chr x = char_matcher (Char.equal x)
    let epsilon s = Success { matched = []; rest = s }
    let empty _ = Failure

    let seq m1 m2 cs =
      match m1 cs with
      | Failure -> Failure
      | Success { matched = matched1; rest } -> (
          match m2 rest with
          | Failure -> Failure
          | Success { matched = matched2; rest } ->
              Success { matched = matched1 @ matched2; rest })

    let alt (m1 : matcher) m2 cs =
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

    let ( >& ) = seq
    let ( >| ) = alt
    let ( ~* ) = kleene
    let plus m = m >& ~*m
    let ( ~+ ) = plus
    let maybe m = m >| epsilon
    let ( ~? ) = maybe

    let from_str s =
      let cs' = String.to_list s in
      List.fold cs' ~init:epsilon ~f:(fun acc -> fun x -> acc >& chr x)
  end

  exception LexFailure

  type token = Lang.token
  type action = char list -> token option
  type lex_state = { lexed : token list; rest : char list }
  type lexer = lex_state -> lex_state outcome
  type matcher = Matcher.matcher

  let tag (m : matcher) to_token { lexed; rest } =
    match m rest with
    | Failure -> Failure
    | Success { matched; rest } -> (
        match to_token matched with
        | None -> Success { lexed; rest }
        | Some t -> Success { lexed = t :: lexed; rest })

  let empty _ = Failure
  let epsilon s = Success s
  let seq l1 l2 s = match l1 s with Failure -> Failure | Success s' -> l2 s'

  let alt l1 l2 s =
    match (l1 s, l2 s) with
    | Failure, Failure -> Failure
    | Success s, Failure -> Success s
    | Failure, Success s' -> Success s'
    | Success s, Success s' ->
        if List.length s.rest <= List.length s'.rest then Success s
        else Success s'

  let rec kleene l s =
    match l s with Failure -> Success s | Success s' -> kleene l s'

  let ( >>& ) = seq
  let ( >>| ) = alt
  let ( ~~* ) = kleene
  let plus l = l >>& ~~*l
  let ( ~~+ ) = plus
  let maybe l = l >>| epsilon
  let ( ~~? ) = maybe

  let lex l s =
    let cs = String.to_list s in
    match l { lexed = []; rest = cs } with
    | Success { lexed; rest = [] } -> List.rev lexed
    | _ -> raise LexFailure
end

open Intfs
open Intfs.Language

module Recogniser = struct
  type recogniser = char list -> char list outcome

  let char_recogniser f cs =
    match cs with
    | [] -> Failure
    | c :: cs -> if f c then Success cs else Failure

  let alphabetic = char_recogniser Base.Char.is_alpha
  let numeric = char_recogniser Base.Char.is_digit
  let alphanumeric = char_recogniser Base.Char.is_alphanum
  let whitespace = char_recogniser Base.Char.is_whitespace
  let chr x = char_recogniser (Base.Char.equal x)
  let empty _ = Failure
  let epsilon cs = Success cs

  let seq (r1 : recogniser) (r2 : recogniser) cs =
    match r1 cs with Failure -> Failure | Success cs -> r2 cs

  let alt (r1 : recogniser) (r2 : recogniser) cs =
    match (r1 cs, r2 cs) with
    | Failure, Failure -> Failure
    | Success xs, Failure -> Success xs
    | Failure, Success ys -> Success ys
    | Success xs, Success ys ->
        if List.length xs <= List.length ys then Success xs else Success ys

  let rec kleene (r : recogniser) cs =
    match r cs with Failure -> Success cs | Success xs -> kleene r xs

  let ( >& ) = seq
  let ( >| ) = alt
  let ( ~* ) = kleene
  let plus (r : recogniser) = r >& ~*r
  let ( ~+ ) = plus
  let maybe (r : recogniser) = r >| epsilon
  let ( ~? ) = maybe

  let from_str s =
    let cs' = Base.String.to_list s in
    Base.List.fold cs' ~init:epsilon ~f:(fun acc -> fun x -> acc >& chr x)

  let from_str_list xs =
    Base.List.fold xs ~init:empty ~f:(fun acc -> fun x -> acc >| from_str x)

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
  type lex_state = { lexed : token list; rest : char list }
  type lexer = lex_state -> lex_state outcome
  type matcher = Matcher.matcher

  let promote (m : matcher) to_token { lexed; rest } =
    match m rest with
    | Failure -> Failure
    | Success { matched; rest } ->
        let t = to_token matched in
        Success { lexed = t :: lexed; rest }

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

  let whitespace { lexed; rest } =
    match Matcher.whitespace rest with
    | Failure -> Failure
    | Success { matched; rest } -> Success { lexed; rest }

  let from_tokens ts = epsilon >>| ts >>& ~~*(whitespace >>& ts)

  let lex l s =
    let cs = String.to_list s in
    match l { lexed = []; rest = cs } with
    | Success { lexed; rest = [] } -> List.rev lexed
    | _ -> raise LexFailure
end

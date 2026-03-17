open Base
open Intfs

type 'a outcome = Success of 'a | Failure

module type Matcher = sig
  type matcher_state = { matched : char list; rest : char list }
  type matcher = char list -> matcher_state outcome

  val ( >& ) : matcher -> matcher -> matcher
  val ( >| ) : matcher -> matcher -> matcher
  val ( ~* ) : matcher -> matcher
  val ( ~+ ) : matcher -> matcher
  val ( ~? ) : matcher -> matcher
  val str : string -> matcher
  val ident : matcher
  val literal : matcher
  val whitespace : matcher
end

module LexerMatcher : Matcher = struct
  type matcher_state = { matched : char list; rest : char list }
  type matcher = char list -> matcher_state outcome

  let match_char f cs =
    match cs with
    | [] -> Failure
    | c :: cs -> if f c then Success { matched = [ c ]; rest = cs } else Failure

  let alphabetic = match_char Char.is_alpha
  let numeric = match_char Char.is_digit
  let alphanumeric = match_char Char.is_alphanum
  let chr x = match_char (Char.equal x)
  let epsilon s = Success { matched = []; rest = s }

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

  let str s =
    let cs' = String.to_list s in
    List.fold cs' ~init:epsilon ~f:(fun acc -> fun x -> acc >& chr x)

  let ident = alphabetic >& ~*alphanumeric
  let literal = ~?(str "-") >& ~+numeric
  let whitespace = match_char Char.is_whitespace
end

module LexerCombinator : Lexer = struct
  open LexerMatcher

  exception LexFailure

  type lex_state = { lexed : token list; rest : char list }

  let promote m ?(ignore = false) to_token { lexed; rest } =
    match m rest with
    | Failure -> Failure
    | Success { matched; rest } ->
        if ignore then Success { lexed; rest }
        else
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

  let ident =
    promote LexerMatcher.ident (fun cs -> IDENT (cs |> String.of_list))

  let literal =
    promote LexerMatcher.literal (fun cs ->
        NUM (cs |> String.of_list |> Int.of_string))

  let whitespace =
    promote LexerMatcher.whitespace (fun _ -> assert false) ~ignore:true

  let keywords =
    [
      ("let", fun _ -> LET);
      ("rec", fun _ -> REC);
      ("in", fun _ -> IN);
      ("fun", fun _ -> FUN);
      ("true", fun _ -> TRUE);
      ("false", fun _ -> FALSE);
    ]

  let operators =
    [
      ("=", fun _ -> EQUALS);
      ("+", fun _ -> PLUS);
      ("->", fun _ -> ARROW);
      ("(", fun _ -> LPARAN);
      (")", fun _ -> RPARAN);
    ]

  let to_lexer xs =
    List.fold xs ~init:empty ~f:(fun acc ->
        fun (s, to_token) -> acc >>| promote (str s) to_token ~ignore:false)

  let lex_one =
    to_lexer keywords >>| to_lexer operators >>| ident >>| literal
    >>| whitespace

  let lex_many = epsilon >>| lex_one >>& ~~*(whitespace >>& lex_one)

  let lex s =
    let cs = String.to_list s in
    match lex_many { lexed = []; rest = cs } with
    | Success { lexed; rest = [] } -> List.rev lexed
    | _ -> raise LexFailure
end

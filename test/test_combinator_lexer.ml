open Compilers.Lexing.Combinators
open Compilers.Intfs
open Compilers.Mlot
open Printf
open Compilers.Mlot.Mlot_Token
open Lexer (Mlot)

let ident_m = Matcher.(alphabetic >& ~*alphanumeric)
let literal_m = Matcher.(~?(from_str "-") >& ~+numeric)
let ident = promote ident_m (fun cs -> IDENT (cs |> Base.String.of_list))

let literal =
  promote literal_m (fun cs ->
      NUM (cs |> Base.String.of_list |> Base.Int.of_string))

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
  Base.List.fold xs ~init:empty ~f:(fun acc ->
      fun (s, to_token) -> acc >>| promote (Matcher.from_str s) to_token)

let tokens = to_lexer keywords >>| to_lexer operators >>| ident >>| literal
let mlot_lexer = from_tokens tokens
let print_token = fun x -> printf "%s ; " (Mlot_Token.to_str x)

let%expect_test _ =
  List.iter print_token (lex mlot_lexer "fun");
  [%expect {| FUN ; |}]

let%expect_test _ =
  List.iter print_token (lex mlot_lexer "fun ->");
  [%expect {| FUN ; ARROW ; |}]

let%expect_test _ =
  List.iter print_token (lex mlot_lexer "fun x -> 2");
  [%expect {| FUN ; IDENT x ; ARROW ; NUM 2 ; |}]

let%expect_test _ =
  List.iter print_token (lex mlot_lexer "fun x2 -> 2");
  [%expect {| FUN ; IDENT x2 ; ARROW ; NUM 2 ; |}]

open Lexparse.Lexing.Combinators
open Lexparse.Intfs
open Lexparse.Mlot
open Mlot_Token
open Printf
open Lexer (Mlot)

let ident_m = Matcher.(alphabetic >& ~*alphanumeric)
let literal_m = Matcher.(~?(from_str "-") >& ~+numeric)
let whitespace_m = Matcher.whitespace
let ident = promote ident_m (fun cs -> Some (IDENT (cs |> Base.String.of_list)))
let whitespace = promote whitespace_m (fun cs -> None)

let literal =
  promote literal_m (fun cs ->
      Some (NUM (cs |> Base.String.of_list |> Base.Int.of_string)))

let keywords =
  [
    ("let", fun _ -> Some LET);
    ("rec", fun _ -> Some REC);
    ("in", fun _ -> Some IN);
    ("fun", fun _ -> Some FUN);
    ("true", fun _ -> Some TRUE);
    ("false", fun _ -> Some FALSE);
  ]

let operators =
  [
    ("=", fun _ -> Some EQUALS);
    ("+", fun _ -> Some PLUS);
    ("->", fun _ -> Some ARROW);
    ("(", fun _ -> Some LPARAN);
    (")", fun _ -> Some RPARAN);
  ]

let to_lexer xs =
  Base.List.fold xs ~init:empty ~f:(fun acc ->
      fun (s, to_token) -> acc >>| promote (Matcher.from_str s) to_token)

let tokens =
  to_lexer keywords >>| to_lexer operators >>| ident >>| literal >>| whitespace

let mlot_lexer = ~~*tokens
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

open Lexparse.Lexing.Generators
open Lexparse.Mlot
open Printf
open Lexer (Mlot) (Mlot_Vocabulary)

let print_token x = printf "%s ; " (Mlot_Token.to_str x)

let%expect_test _ =
  List.iter print_token (lex "x");
  [%expect {| IDENT x ; |}]

let%expect_test _ =
  List.iter print_token (lex "fun");
  [%expect {| FUN ; |}]

let%expect_test _ =
  List.iter print_token (lex "fun ->");
  [%expect {| FUN ; ARROW ; |}]

let%expect_test _ =
  List.iter print_token (lex "fun x -> 2");
  [%expect {| FUN ; IDENT x ; ARROW ; NUM 2 ; |}]

let%expect_test _ =
  List.iter print_token (lex "fun x2 -> 2");
  [%expect {| FUN ; IDENT x2 ; ARROW ; NUM 2 ; |}]

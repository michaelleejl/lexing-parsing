open Lexparse.Intfs.Tags
open Lexparse.Lexing.Generators
open Lexparse.Mlot
open Mlot_Token
open Printf
open Lexer (Mlot) (LexTag)
open Lexparse.Regex

let keywords =
  compile (r "let") T_LET
  >>| compile (r "rec") T_REC
  >>| compile (r "in") T_IN
  >>| compile (r "fun") T_FUN
  >>| compile (r "true") T_TRUE
  >>| compile (r "false") T_FALSE

let operators =
  compile (r "=") T_EQUALS
  >>| compile (r {|\+|}) T_PLUS
  >>| compile (r "->") T_ARROW
  >>| compile (r {|\(|}) T_LPAREN
  >>| compile (r {|\)|}) T_RPAREN

let ident = compile (r "[a-zA-Z][a-zA-Z0-9]*") T_IDENT
let literal = compile (r "-?[0-9]+") T_NUM
let whitespace = compile (r {|\s|}) T_SKIP
let mlot_lexer_nfa = keywords >>| operators >>| ident >>| literal >>| whitespace
let mlot_lexer = determinise mlot_lexer_nfa
let print_token x = printf "%s ; " (Mlot_Token.to_str x)

let%expect_test _ =
  List.iter print_token (lex mlot_lexer "x");
  [%expect {| IDENT x ; |}]

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

open Lexparse.Intfs.Tags
open Lexparse.Lexing.Generators
open Lexparse.Mlot
open Mlot_Token
open Printf
open Lexer (Mlot)
open Lexparse.Regex

let keywords =
  compile (r "let") (fun _ -> Some LET)
  >>| compile (r "rec") (fun _ -> Some REC)
  >>| compile (r "in") (fun _ -> Some IN)
  >>| compile (r "fun") (fun _ -> Some FUN)
  >>| compile (r "true") (fun _ -> Some TRUE)
  >>| compile (r "false") (fun _ -> Some FALSE)

let operators =
  compile (r "=") (fun _ -> Some EQUALS)
  >>| compile (r {|\+|}) (fun _ -> Some PLUS)
  >>| compile (r "->") (fun _ -> Some ARROW)
  >>| compile (r {|\(|}) (fun _ -> Some LPAREN)
  >>| compile (r {|\)|}) (fun _ -> Some RPAREN)

let ident = compile (r "[a-zA-Z][a-zA-Z0-9]*") (fun cs -> Some (IDENT (Base.String.of_list cs)))
let literal = compile (r "-?[0-9]+") (fun cs -> Some (NUM (cs |> Base.String.of_list |> Base.Int.of_string)))
let whitespace = compile (r {|\s|}) (fun _ -> None)
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

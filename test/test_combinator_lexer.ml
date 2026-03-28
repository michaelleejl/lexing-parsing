open Lexparse.Lexing.Combinators
open Lexparse.Intfs
open Lexparse.Mlot
open Mlot_Token
open Printf
open Lexer (Mlot)
open Lexparse.Regex

let keywords =
  interpret (r "let") (fun _ -> Some LET)
  >>| interpret (r "rec") (fun _ -> Some REC)
  >>| interpret (r "in") (fun _ -> Some IN)
  >>| interpret (r "fun") (fun _ -> Some FUN)
  >>| interpret (r "true") (fun _ -> Some TRUE)
  >>| interpret (r "false") (fun _ -> Some FALSE)

let operators =
  interpret (r "=") (fun _ -> Some EQUALS)
  >>| interpret (r {|\+|}) (fun _ -> Some PLUS)
  >>| interpret (r "->") (fun _ -> Some ARROW)
  >>| interpret (r {|\(|}) (fun _ -> Some LPAREN)
  >>| interpret (r {|\)|}) (fun _ -> Some RPAREN)

let ident =
  interpret (r "[a-zA-Z][a-zA-Z0-9]*") (fun cs ->
      Some (IDENT (Base.String.of_list cs)))

let literal =
  interpret (r "-?[0-9]+") (fun cs ->
      Some (NUM (cs |> Base.String.of_list |> Base.Int.of_string)))

let whitespace = interpret (r {|\s|}) (fun _ -> None)
let mlot_lexer = keywords >>| operators >>| ident >>| literal >>| whitespace
let print_token x = printf "%s ; " (Mlot_Token.to_str x)

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

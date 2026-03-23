open Lexparse.Intfs.Tags
open Lexparse.Lexing.Generators
open Lexparse.Mlot
open Mlot_Token
open Printf

module Tag = struct
  type t =
    | T_TRUE
    | T_FALSE
    | T_FUN
    | T_ARROW
    | T_LPARAN
    | T_RPARAN
    | T_PLUS
    | T_LET
    | T_EQUALS
    | T_IN
    | T_REC
    | T_IDENT
    | T_NUM
    | T_SKIP

  type token = Mlot_Token.t

  let compare = compare

  let tag_to_action = function
    | T_TRUE -> fun _ -> Some TRUE
    | T_FALSE -> fun _ -> Some FALSE
    | T_FUN -> fun _ -> Some FUN
    | T_ARROW -> fun _ -> Some ARROW
    | T_LPARAN -> fun _ -> Some LPARAN
    | T_RPARAN -> fun _ -> Some RPARAN
    | T_PLUS -> fun _ -> Some PLUS
    | T_LET -> fun _ -> Some LET
    | T_EQUALS -> fun _ -> Some EQUALS
    | T_IN -> fun _ -> Some IN
    | T_REC -> fun _ -> Some REC
    | T_IDENT -> fun cs -> Some (IDENT (Base.String.of_list cs))
    | T_NUM ->
        fun cs -> Some (NUM (cs |> Base.String.of_list |> Base.Int.of_string))
    | T_SKIP -> fun _ -> None
end

open Lexer (Mlot) (Tag)
open Matcher

let keywords =
  tag (r "let") T_LET
  >>| tag (r "rec") T_REC
  >>| tag (r "in") T_IN
  >>| tag (r "fun") T_FUN
  >>| tag (r "true") T_TRUE
  >>| tag (r "false") T_FALSE

let operators =
  tag (r "=") T_EQUALS
  >>| tag (r {|\+|}) T_PLUS
  >>| tag (r "->") T_ARROW
  >>| tag (r {|\(|}) T_LPARAN
  >>| tag (r {|\)|}) T_RPARAN

let ident = tag (r "[a-zA-Z][a-zA-Z0-9]*") T_IDENT
let literal = tag (r "-?[0-9]+") T_NUM
let whitespace = tag (r {|\s|}) T_SKIP
let mlot_lexer_nfa = keywords >>| operators >>| ident >>| literal >>| whitespace
let mlot_lexer = compile mlot_lexer_nfa
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

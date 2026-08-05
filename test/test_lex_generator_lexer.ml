open Lexparse.Lexing.Generators
open Lexparse.Mlot
open Lexer (Vocabulary)

let%expect_test "corpus" =
  Lex_cases.run lex;
  [%expect
    {|
    ""
    " "
    "x"                    IDENT x
    "x1"                   IDENT x1
    "42"                   NUM 42
    "-123"                 NUM -123
    "true"                 TRUE
    "false"                FALSE
    "fun"                  FUN
    "fun ->"               FUN ; ARROW
    "fun x -> 2"           FUN ; IDENT x ; ARROW ; NUM 2
    "fun x2 -> 2"          FUN ; IDENT x2 ; ARROW ; NUM 2
    "fun x -> x"           FUN ; IDENT x ; ARROW ; IDENT x
    "fun  x   ->   x"      FUN ; IDENT x ; ARROW ; IDENT x
    "fun\tx"               FUN ; IDENT x
    "fun\nx"               FUN ; IDENT x
    "let x = 1 in x"       LET ; IDENT x ; EQUALS ; NUM 1 ; IN ; IDENT x
    "let x=1 in x"         LET ; IDENT x ; EQUALS ; NUM 1 ; IN ; IDENT x
    "let rec f = fun x -> f x in f 1" LET ; REC ; IDENT f ; EQUALS ; FUN ; IDENT x ; ARROW ; IDENT f ; IDENT x ; IN ; IDENT f ; NUM 1
    "(1 + 2)"              LPAREN ; NUM 1 ; PLUS ; NUM 2 ; RPAREN
    "1 + -2"               NUM 1 ; PLUS ; NUM -2
    "f (x)"                IDENT f ; LPAREN ; IDENT x ; RPAREN
    "letrec"               IDENT letrec
    "trueish"              IDENT trueish
    "inx"                  IDENT inx
    "fun->x"               FUN ; ARROW ; IDENT x
    "1+2"                  NUM 1 ; PLUS ; NUM 2
    "x=y"                  IDENT x ; EQUALS ; IDENT y
    "@"                    <lex error>
    "x @ y"                <lex error>
    "_x"                   <lex error>
    "1a"                   NUM 1 ; IDENT a
    |}]

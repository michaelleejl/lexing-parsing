open Lexparse.Mlot
open Lexparse.Intfs.BNF
open Lexparse.Parsing.Combinators
open Printf
open Mlot_Token
open Descent (Mlot) (Mlot_Grammar)

let print_ast x = Mlot_Ast.to_str x |> printf "%s"

let%expect_test _ =
  print_ast (parse [ Mlot_Token.FUN; IDENT "x"; ARROW; IDENT "x"; PLUS; NUM 0 ]);
  [%expect {| Fun(x, Plus(x, 0)) |}]

let%expect_test _ =
  print_ast (parse [ Mlot_Token.NUM 1; PLUS; NUM 2; PLUS; NUM 3 ]);
  [%expect {| Plus(Plus(1, 2), 3) |}]

let%expect_test _ =
  print_ast
    (parse [ Mlot_Token.NUM 1; PLUS; LPAREN; NUM 2; PLUS; NUM 3; RPAREN ]);
  [%expect {| Plus(1, Plus(2, 3)) |}]

let%expect_test _ =
  print_ast (parse [ Mlot_Token.NUM 1; PLUS; NUM 2; EQUALS; NUM 3 ]);
  [%expect {| Equals(Plus(1, 2), 3) |}]

let%expect_test _ =
  print_ast (parse [ Mlot_Token.IDENT "f"; LPAREN; NUM 1; PLUS; NUM 2; RPAREN ]);
  [%expect {| App(f, Plus(1, 2)) |}]

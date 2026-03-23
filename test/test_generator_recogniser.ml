open Lexparse.Lexing.Generators.Recogniser
open Printf

let keywords = r "let|rec|in|fun|true|false"
let operators = r {|=|\+|->|(|)|}
let ident = r "[a-zA-Z][a-zA-Z0-9]*"
let literal = r "-?[0-9]+"
let whitespace = r {|\s|}
let recognise_one = keywords >| operators >| ident >| literal

let mlot_recogniser =
  compile (epsilon >| recognise_one >& ~*(whitespace >& recognise_one))

let%expect_test _ =
  printf "%b" (recognise mlot_recogniser "fun");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise mlot_recogniser "fun ->");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise mlot_recogniser "x");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise mlot_recogniser "-123");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise mlot_recogniser "0123");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise mlot_recogniser "fun->");
  [%expect {| false |}]

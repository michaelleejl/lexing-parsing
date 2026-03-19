open Compilers.Lexers.Recogniser
open Printf

let keywords = [ "let"; "rec"; "in"; "fun"; "true"; "false" ]
let operators = [ "="; "+"; "->"; "("; ")" ]
let ident = alphabetic >& ~*alphanumeric
let literal = ~?(from_str "-") >& ~+numeric

let recognise_one =
     from_str_list keywords
  >| from_str_list operators 
  >| ident
  >| literal
  >| whitespace

let mlot_recogniser =
     epsilon 
  >| recognise_one >& ~*(whitespace >& recognise_one)

let%expect_test _ =
  printf "%b" (recognise mlot_recogniser "fun");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise mlot_recogniser " ");
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

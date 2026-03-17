open Compilers.Lexers.Recogniser
open Printf

let%expect_test _ =
  printf "%b" (recognise "fun");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise " ");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise "fun ->");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise "x");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise "-123");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise "0123");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (recognise "fun->");
  [%expect {| false |}]

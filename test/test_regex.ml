open Compilers.Nfa
open Compilers.Regex
open Printf

let r = "[a-bd-f]|c*g"
let n = compile (parse r) 

let%expect_test _ =
  printf "%b" (accept n "");
  [%expect {| false |}] 

  let%expect_test _ =
  printf "%b" (accept n "b");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept n "a");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept n "d");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept n "e");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept n "f");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept n "c");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept n "g");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept n "cg");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept n "cccg");
  [%expect {| true |}]
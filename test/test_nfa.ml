open Compilers.Nfa
open Printf

let%expect_test _ =
  printf "%b" (accept empty "");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept epsilon "");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept epsilon "c");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept (one_of ['a'; 'b'; 'c']) "a");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept (one_of ['a'; 'b'; 'c']) "b");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept (one_of ['a'; 'b'; 'c']) "c");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept (one_of ['a'; 'b'; 'c']) "d");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept (one_of ['a'; 'b'; 'c']) "");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept (alt (one_of ['a']) (one_of ['b'])) "");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept (alt (one_of ['a']) (one_of ['b'])) "a");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept (alt (one_of ['a']) (one_of ['b'])) "b");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept (alt (one_of ['a']) (one_of ['b'])) "c");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept (alt (one_of ['a']) (one_of ['b'])) "ab");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept (seq (one_of ['a']) (one_of ['b'])) "ab");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept (seq (one_of ['a']) (one_of ['b'])) "a");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept (seq (one_of ['a']) (one_of ['b'])) "b");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept (seq (one_of ['a']) (one_of ['b'])) "c");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept (kleene (one_of ['a'])) "");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept (kleene (one_of ['a'])) "a");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept (kleene (one_of ['a'])) "aa");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept (kleene (one_of ['a'])) "aaa");
  [%expect {| true |}]

let%expect_test _ =
  printf "%b" (accept (kleene (one_of ['a'])) "aaab");
  [%expect {| false |}]

let%expect_test _ =
  printf "%b" (accept (seq (kleene (one_of ['a'])) (one_of ['b'])) "aaab");
  [%expect {| true |}]
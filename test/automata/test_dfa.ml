open Printf
module Dfa = Lexparse.Automata.Dfa.Make (Char)
module Nfa = Dfa.Nfa
open Nfa

let automata =
  [
    ("empty", empty);
    ("epsilon", epsilon);
    ("[abc]", one_of [ 'a'; 'b'; 'c' ]);
    ("a|b", alt (one_of [ 'a' ]) (one_of [ 'b' ]));
    ("ab", seq (one_of [ 'a' ]) (one_of [ 'b' ]));
    ("a*", kleene (one_of [ 'a' ]));
    ("a*b", seq (kleene (one_of [ 'a' ])) (one_of [ 'b' ]));
    ("(a|b)*", kleene (alt (one_of [ 'a' ]) (one_of [ 'b' ])));
    ("(ab)*", kleene (seq (one_of [ 'a' ]) (one_of [ 'b' ])));
    ( "a(b|c)*d",
      seq
        (seq (one_of [ 'a' ]) (kleene (alt (one_of [ 'b' ]) (one_of [ 'c' ]))))
        (one_of [ 'd' ]) );
    ("(a*)*", kleene (kleene (one_of [ 'a' ])));
  ]

let inputs =
  [ ""; "a"; "b"; "c"; "d"; "ab"; "ba"; "aa"; "aab"; "abcd"; "abbcd" ]

let%expect_test "determinise preserves the language" =
  List.iter
    (fun (label, nfa) ->
      let dfa = Dfa.determinise nfa in
      List.iter
        (fun s ->
          let cs = List.init (String.length s) (String.get s) in
          let n = accept nfa cs and d = Dfa.accept dfa cs in
          printf "%-10s %-8s nfa=%-5b dfa=%-5b%s\n" label (sprintf "%S" s) n d
            (if n = d then "" else "  MISMATCH"))
        inputs)
    automata;
  [%expect
    {|
    empty      ""       nfa=false dfa=false
    empty      "a"      nfa=false dfa=false
    empty      "b"      nfa=false dfa=false
    empty      "c"      nfa=false dfa=false
    empty      "d"      nfa=false dfa=false
    empty      "ab"     nfa=false dfa=false
    empty      "ba"     nfa=false dfa=false
    empty      "aa"     nfa=false dfa=false
    empty      "aab"    nfa=false dfa=false
    empty      "abcd"   nfa=false dfa=false
    empty      "abbcd"  nfa=false dfa=false
    epsilon    ""       nfa=true  dfa=true
    epsilon    "a"      nfa=false dfa=false
    epsilon    "b"      nfa=false dfa=false
    epsilon    "c"      nfa=false dfa=false
    epsilon    "d"      nfa=false dfa=false
    epsilon    "ab"     nfa=false dfa=false
    epsilon    "ba"     nfa=false dfa=false
    epsilon    "aa"     nfa=false dfa=false
    epsilon    "aab"    nfa=false dfa=false
    epsilon    "abcd"   nfa=false dfa=false
    epsilon    "abbcd"  nfa=false dfa=false
    [abc]      ""       nfa=false dfa=false
    [abc]      "a"      nfa=true  dfa=true
    [abc]      "b"      nfa=true  dfa=true
    [abc]      "c"      nfa=true  dfa=true
    [abc]      "d"      nfa=false dfa=false
    [abc]      "ab"     nfa=false dfa=false
    [abc]      "ba"     nfa=false dfa=false
    [abc]      "aa"     nfa=false dfa=false
    [abc]      "aab"    nfa=false dfa=false
    [abc]      "abcd"   nfa=false dfa=false
    [abc]      "abbcd"  nfa=false dfa=false
    a|b        ""       nfa=false dfa=false
    a|b        "a"      nfa=true  dfa=true
    a|b        "b"      nfa=true  dfa=true
    a|b        "c"      nfa=false dfa=false
    a|b        "d"      nfa=false dfa=false
    a|b        "ab"     nfa=false dfa=false
    a|b        "ba"     nfa=false dfa=false
    a|b        "aa"     nfa=false dfa=false
    a|b        "aab"    nfa=false dfa=false
    a|b        "abcd"   nfa=false dfa=false
    a|b        "abbcd"  nfa=false dfa=false
    ab         ""       nfa=false dfa=false
    ab         "a"      nfa=false dfa=false
    ab         "b"      nfa=false dfa=false
    ab         "c"      nfa=false dfa=false
    ab         "d"      nfa=false dfa=false
    ab         "ab"     nfa=true  dfa=true
    ab         "ba"     nfa=false dfa=false
    ab         "aa"     nfa=false dfa=false
    ab         "aab"    nfa=false dfa=false
    ab         "abcd"   nfa=false dfa=false
    ab         "abbcd"  nfa=false dfa=false
    a*         ""       nfa=true  dfa=true
    a*         "a"      nfa=true  dfa=true
    a*         "b"      nfa=false dfa=false
    a*         "c"      nfa=false dfa=false
    a*         "d"      nfa=false dfa=false
    a*         "ab"     nfa=false dfa=false
    a*         "ba"     nfa=false dfa=false
    a*         "aa"     nfa=true  dfa=true
    a*         "aab"    nfa=false dfa=false
    a*         "abcd"   nfa=false dfa=false
    a*         "abbcd"  nfa=false dfa=false
    a*b        ""       nfa=false dfa=false
    a*b        "a"      nfa=false dfa=false
    a*b        "b"      nfa=true  dfa=true
    a*b        "c"      nfa=false dfa=false
    a*b        "d"      nfa=false dfa=false
    a*b        "ab"     nfa=true  dfa=true
    a*b        "ba"     nfa=false dfa=false
    a*b        "aa"     nfa=false dfa=false
    a*b        "aab"    nfa=true  dfa=true
    a*b        "abcd"   nfa=false dfa=false
    a*b        "abbcd"  nfa=false dfa=false
    (a|b)*     ""       nfa=true  dfa=true
    (a|b)*     "a"      nfa=true  dfa=true
    (a|b)*     "b"      nfa=true  dfa=true
    (a|b)*     "c"      nfa=false dfa=false
    (a|b)*     "d"      nfa=false dfa=false
    (a|b)*     "ab"     nfa=true  dfa=true
    (a|b)*     "ba"     nfa=true  dfa=true
    (a|b)*     "aa"     nfa=true  dfa=true
    (a|b)*     "aab"    nfa=true  dfa=true
    (a|b)*     "abcd"   nfa=false dfa=false
    (a|b)*     "abbcd"  nfa=false dfa=false
    (ab)*      ""       nfa=true  dfa=true
    (ab)*      "a"      nfa=false dfa=false
    (ab)*      "b"      nfa=false dfa=false
    (ab)*      "c"      nfa=false dfa=false
    (ab)*      "d"      nfa=false dfa=false
    (ab)*      "ab"     nfa=true  dfa=true
    (ab)*      "ba"     nfa=false dfa=false
    (ab)*      "aa"     nfa=false dfa=false
    (ab)*      "aab"    nfa=false dfa=false
    (ab)*      "abcd"   nfa=false dfa=false
    (ab)*      "abbcd"  nfa=false dfa=false
    a(b|c)*d   ""       nfa=false dfa=false
    a(b|c)*d   "a"      nfa=false dfa=false
    a(b|c)*d   "b"      nfa=false dfa=false
    a(b|c)*d   "c"      nfa=false dfa=false
    a(b|c)*d   "d"      nfa=false dfa=false
    a(b|c)*d   "ab"     nfa=false dfa=false
    a(b|c)*d   "ba"     nfa=false dfa=false
    a(b|c)*d   "aa"     nfa=false dfa=false
    a(b|c)*d   "aab"    nfa=false dfa=false
    a(b|c)*d   "abcd"   nfa=true  dfa=true
    a(b|c)*d   "abbcd"  nfa=true  dfa=true
    (a*)*      ""       nfa=true  dfa=true
    (a*)*      "a"      nfa=true  dfa=true
    (a*)*      "b"      nfa=false dfa=false
    (a*)*      "c"      nfa=false dfa=false
    (a*)*      "d"      nfa=false dfa=false
    (a*)*      "ab"     nfa=false dfa=false
    (a*)*      "ba"     nfa=false dfa=false
    (a*)*      "aa"     nfa=true  dfa=true
    (a*)*      "aab"    nfa=false dfa=false
    (a*)*      "abcd"   nfa=false dfa=false
    (a*)*      "abbcd"  nfa=false dfa=false
    |}]

let%expect_test "stepping an nfa by hand" =
  let nfa = seq (kleene (one_of [ 'a' ])) (one_of [ 'b' ]) in
  let states = ref (initialise nfa) in
  List.iter
    (fun c ->
      states := step nfa !states c;
      printf "after %c: accepting=%b rejecting=%b\n" c
        (is_accepting nfa !states) (is_rejecting nfa !states))
    [ 'a'; 'a'; 'b' ];
  [%expect
    {|
    after a: accepting=false rejecting=false
    after a: accepting=false rejecting=false
    after b: accepting=true rejecting=false
    |}]

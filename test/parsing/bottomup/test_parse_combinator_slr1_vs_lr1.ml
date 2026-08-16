open Lexparse.Parsing.Bottomup.Combinators
open Pointer_grammar

module Slr1 = SLR1 (Pointer_grammar)
module Lr1 = LR1 (Pointer_grammar)

let cases =
  [
    ("id", [ ID "x" ]);
    ("* id", [ STAR; ID "x" ]);
    ("* * id", [ STAR; STAR; ID "x" ]);
    ("id = id", [ ID "x"; EQ; ID "y" ]);
    ("* id = id", [ STAR; ID "x"; EQ; ID "y" ]);
    ("id = * id", [ ID "x"; EQ; STAR; ID "y" ]);
    ("<empty>", []);
    ("id id", [ ID "x"; ID "y" ]);
    ("= id", [ EQ; ID "y" ]);
    ("id =", [ ID "x"; EQ ]);
    ("*", [ STAR ]);
    ("id = id = id", [ ID "x"; EQ; ID "y"; EQ; ID "z" ]);
  ]

let describe parse toks =
  match parse toks with
  | ast -> ast
  | exception e ->
      let s = Printexc.to_string e in
      let i = match String.rindex_opt s '.' with Some i -> i + 1 | None -> 0 in
      "<" ^ String.sub s i (String.length s - i) ^ ">"

let%expect_test "LR(1) parses what SLR(1) sees as a conflict" =
  Printf.printf "%-14s %-42s %s\n" "input" "SLR1" "LR1";
  List.iter
    (fun (src, toks) ->
      Printf.printf "%-14s %-42s %s\n" src
        (describe Slr1.parse toks)
        (describe Lr1.parse toks))
    cases;
  [%expect {|
    input          SLR1                                       LR1
    id             x                                          x
    * id           Deref(x)                                   Deref(x)
    * * id         Deref(Deref(x))                            Deref(Deref(x))
    id = id        <ParseError("shift reduce conflict")>      Assign(x, y)
    * id = id      <ParseError("shift reduce conflict")>      Assign(Deref(x), y)
    id = * id      <ParseError("shift reduce conflict")>      Assign(x, Deref(y))
    <empty>        <ParseError("no actions")>                 <ParseError("no actions")>
    id id          <ParseError("no actions")>                 <ParseError("no actions")>
    = id           <ParseError("no actions")>                 <ParseError("no actions")>
    id =           <ParseError("shift reduce conflict")>      <ParseError("no actions")>
    *              <ParseError("no actions")>                 <ParseError("no actions")>
    id = id = id   <ParseError("shift reduce conflict")>      <ParseError("no actions")>
    |}]

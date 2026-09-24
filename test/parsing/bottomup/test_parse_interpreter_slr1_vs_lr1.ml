open Lexparse.Parsing.Bottomup.Interpreters
open Pointer_grammar
module Slr1 = Slr1 (Pointer_grammar)
module Lr1 = Lr1 (Pointer_grammar)

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
      let i =
        match String.rindex_opt s '.' with Some i -> i + 1 | None -> 0
      in
      "<" ^ String.sub s i (String.length s - i) ^ ">"

let%expect_test "LR(1) parses what SLR(1) sees as a conflict" =
  Printf.printf "%-14s %-42s %s\n" "input" "Slr1" "Lr1";
  List.iter
    (fun (src, toks) ->
      Printf.printf "%-14s %-42s %s\n" src (describe Slr1.parse toks)
        (describe Lr1.parse toks))
    cases;
  [%expect
    {|
    input          Slr1                                       Lr1
    id             x                                          x
    * id           Deref(x)                                   Deref(x)
    * * id         Deref(Deref(x))                            Deref(Deref(x))
    id = id        <Parse_error("shift reduce conflict")>     Assign(x, y)
    * id = id      <Parse_error("shift reduce conflict")>     Assign(Deref(x), y)
    id = * id      <Parse_error("shift reduce conflict")>     Assign(x, Deref(y))
    <empty>        <Parse_error("no actions")>                <Parse_error("no actions")>
    id id          <Parse_error("no actions")>                <Parse_error("no actions")>
    = id           <Parse_error("no actions")>                <Parse_error("no actions")>
    id =           <Parse_error("shift reduce conflict")>     <Parse_error("no actions")>
    *              <Parse_error("no actions")>                <Parse_error("no actions")>
    id = id = id   <Parse_error("shift reduce conflict")>     <Parse_error("no actions")>
    |}]

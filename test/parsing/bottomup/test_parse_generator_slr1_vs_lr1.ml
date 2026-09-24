open Lexparse.Parsing.Bottomup.Generators
open Pointer_grammar


module type PARSER = sig
  val parse : token list -> ast
end

let slr1 () = (module SLR1 (Pointer_grammar) : PARSER)
let lr1 () = (module LR1 (Pointer_grammar) : PARSER)

(* Report an exception by its constructor alone.  The conflict is raised by
   [Tables.Action(State).Conflict], whose [State] argument is sealed inside
   the generator, so the test cannot name it to match on it. *)
let render_exn e =
  let s = Printexc.to_string e in
  let i = match String.rindex_opt s '.' with Some i -> i + 1 | None -> 0 in
  "<" ^ String.sub s i (String.length s - i) ^ ">"

let generate name build =
  match build () with
  | (_ : (module PARSER)) -> Printf.printf "%-6s %s\n" name "table built"
  | exception e -> Printf.printf "%-6s %s\n" name (render_exn e)

let%expect_test "SLR(1) cannot build a table for the pointer grammar" =
  generate "SLR1" slr1;
  generate "LR1" lr1;
  [%expect {|
    SLR1   <Conflict>
    LR1    table built
    |}]

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

let%expect_test "LR(1) parses the pointer grammar" =
  let (module P) = lr1 () in
  let describe toks =
    match P.parse toks with ast -> ast | exception e -> render_exn e
  in
  Printf.printf "%-14s %s\n" "input" "LR1";
  List.iter
    (fun (src, toks) -> Printf.printf "%-14s %s\n" src (describe toks))
    cases;
  [%expect {|
    input          LR1
    id             x
    * id           Deref(x)
    * * id         Deref(Deref(x))
    id = id        Assign(x, y)
    * id = id      Assign(Deref(x), y)
    id = * id      Assign(x, Deref(y))
    <empty>        <No_action>
    id id          <No_action>
    = id           <No_action>
    id =           <No_action>
    *              <No_action>
    id = id = id   <No_action>
    |}]

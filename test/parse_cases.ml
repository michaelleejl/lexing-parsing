open Printf
open Lexparse.Mlot
open Token

(* One corpus, run against every parser implementation. The six parsers are
   built from two grammars by four different techniques, but they accept the
   same language and build the same trees, so a disagreement between them is a
   bug in one of them — and shows up here as a diff in a single expect block. *)

let accepted =
  [
    ("x", [ IDENT "x" ]);
    ("42", [ NUM 42 ]);
    ("true", [ TRUE ]);
    ("false", [ FALSE ]);
    ("x + y", [ IDENT "x"; PLUS; IDENT "y" ]);
    ("1 + 2 + 3", [ NUM 1; PLUS; NUM 2; PLUS; NUM 3 ]);
    ("1 = 2 = 3", [ NUM 1; EQUALS; NUM 2; EQUALS; NUM 3 ]);
    ("1 + 2 = 3", [ NUM 1; PLUS; NUM 2; EQUALS; NUM 3 ]);
    ("1 + 2 = 3 + 4", [ NUM 1; PLUS; NUM 2; EQUALS; NUM 3; PLUS; NUM 4 ]);
    ("f x", [ IDENT "f"; IDENT "x" ]);
    ("f x y", [ IDENT "f"; IDENT "x"; IDENT "y" ]);
    ("f x + g y", [ IDENT "f"; IDENT "x"; PLUS; IDENT "g"; IDENT "y" ]);
    ("1 2", [ NUM 1; NUM 2 ]);
    ("(1 + 2) + 3", [ LPAREN; NUM 1; PLUS; NUM 2; RPAREN; PLUS; NUM 3 ]);
    ("1 + (2 + 3)", [ NUM 1; PLUS; LPAREN; NUM 2; PLUS; NUM 3; RPAREN ]);
    ("f (1 + 2)", [ IDENT "f"; LPAREN; NUM 1; PLUS; NUM 2; RPAREN ]);
    ("((x))", [ LPAREN; LPAREN; IDENT "x"; RPAREN; RPAREN ]);
    ("fun x -> x", [ FUN; IDENT "x"; ARROW; IDENT "x" ]);
    ("fun x -> x + 1", [ FUN; IDENT "x"; ARROW; IDENT "x"; PLUS; NUM 1 ]);
    ( "fun x -> fun y -> x + y",
      [
        FUN; IDENT "x"; ARROW; FUN; IDENT "y"; ARROW; IDENT "x"; PLUS; IDENT "y";
      ] );
    ( "(fun x -> x) 1",
      [ LPAREN; FUN; IDENT "x"; ARROW; IDENT "x"; RPAREN; NUM 1 ] );
    ( "let x = 1 in x + 2",
      [ LET; IDENT "x"; EQUALS; NUM 1; IN; IDENT "x"; PLUS; NUM 2 ] );
    ( "let x = 1 in let y = 2 in x + y",
      [
        LET;
        IDENT "x";
        EQUALS;
        NUM 1;
        IN;
        LET;
        IDENT "y";
        EQUALS;
        NUM 2;
        IN;
        IDENT "x";
        PLUS;
        IDENT "y";
      ] );
    ( "let rec f = fun x -> f x in f 1",
      [
        LET;
        REC;
        IDENT "f";
        EQUALS;
        FUN;
        IDENT "x";
        ARROW;
        IDENT "f";
        IDENT "x";
        IN;
        IDENT "f";
        NUM 1;
      ] );
    ( "let x = fun y -> y in x 1",
      [
        LET;
        IDENT "x";
        EQUALS;
        FUN;
        IDENT "y";
        ARROW;
        IDENT "y";
        IN;
        IDENT "x";
        NUM 1;
      ] );
    ( "(let x = 1 in x) + 2",
      [
        LPAREN;
        LET;
        IDENT "x";
        EQUALS;
        NUM 1;
        IN;
        IDENT "x";
        RPAREN;
        PLUS;
        NUM 2;
      ] );
    ( "fun x -> let y = x in y",
      [
        FUN; IDENT "x"; ARROW; LET; IDENT "y"; EQUALS; IDENT "x"; IN; IDENT "y";
      ] );
  ]

let rejected =
  [
    ("<empty>", []);
    ("1 +", [ NUM 1; PLUS ]);
    ("+ 1", [ PLUS; NUM 1 ]);
    ("x =", [ IDENT "x"; EQUALS ]);
    ("(1", [ LPAREN; NUM 1 ]);
    ("1)", [ NUM 1; RPAREN ]);
    ("()", [ LPAREN; RPAREN ]);
    ("in", [ IN ]);
    ("fun -> x", [ FUN; ARROW; IDENT "x" ]);
    ("fun x x", [ FUN; IDENT "x"; IDENT "x" ]);
    ("let x 1 in x", [ LET; IDENT "x"; NUM 1; IN; IDENT "x" ]);
    ("let x = 1 x", [ LET; IDENT "x"; EQUALS; NUM 1; IDENT "x" ]);
    ("rec x = 1 in x", [ REC; IDENT "x"; EQUALS; NUM 1; IN; IDENT "x" ]);
    (* [fun] and [let] are expressions, not operands *)
    ("1 + fun x -> x", [ NUM 1; PLUS; FUN; IDENT "x"; ARROW; IDENT "x" ]);
    ( "f let x = 1 in x",
      [ IDENT "f"; LET; IDENT "x"; EQUALS; NUM 1; IN; IDENT "x" ] );
  ]

let cases = accepted @ rejected

(* The parsers' [ParseFail] exceptions are not exported from their .mli files,
   so a rejection is recognised by elimination: anything that is not one of the
   runtime's own failures is the parser saying no. Crashes stay distinguishable
   — a [Not_found] out of a table lookup is not a rejection. *)
let describe = function
  | Not_found -> "<crash: Not_found>"
  | Match_failure _ -> "<crash: Match_failure>"
  | Assert_failure _ -> "<crash: Assert_failure>"
  | Invalid_argument m -> sprintf "<crash: Invalid_argument %s>" m
  | Failure m -> sprintf "<crash: Failure %s>" m
  | Stack_overflow -> "<crash: Stack_overflow>"
  | _ -> "<rejected>"

let run parse =
  List.iter
    (fun (src, toks) ->
      let result = try Ast.to_str (parse toks) with e -> describe e in
      printf "%-32s %s\n" src result)
    cases

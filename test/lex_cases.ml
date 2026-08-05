open Printf
open Lexparse.Mlot

(* One corpus, run against both lexer implementations. As with the parsers,
   the interpreted and the compiled lexer are supposed to agree token for
   token, so a divergence lands as a diff in one expect block. *)

let cases =
  [
    "";
    " ";
    "x";
    "x1";
    "42";
    "-123";
    "true";
    "false";
    "fun";
    "fun ->";
    "fun x -> 2";
    "fun x2 -> 2";
    "fun x -> x";
    "fun  x   ->   x";
    "fun\tx";
    "fun\nx";
    "let x = 1 in x";
    "let x=1 in x";
    "let rec f = fun x -> f x in f 1";
    "(1 + 2)";
    "1 + -2";
    "f (x)";
    (* maximal munch: neither of these is a keyword followed by a suffix *)
    "letrec";
    "trueish";
    "inx";
    (* no whitespace between adjacent tokens *)
    "fun->x";
    "1+2";
    "x=y";
    (* outside the vocabulary *)
    "@";
    "x @ y";
    "_x";
    "1a";
  ]

(* [LexFailure] is declared with a different arity by the two lexers, so it
   cannot be named once for both; recognise a rejection by elimination and keep
   genuine crashes distinguishable. *)
let describe = function
  | Not_found -> "<crash: Not_found>"
  | Match_failure _ -> "<crash: Match_failure>"
  | Assert_failure _ -> "<crash: Assert_failure>"
  | Invalid_argument m -> sprintf "<crash: Invalid_argument %s>" m
  | Failure m -> sprintf "<crash: Failure %s>" m
  | Stack_overflow -> "<crash: Stack_overflow>"
  | _ -> "<lex error>"

let run lex =
  List.iter
    (fun src ->
      let out =
        try lex src |> List.map Token.to_str |> String.concat " ; "
        with e -> describe e
      in
      printf "%-22s %s\n" (sprintf "%S" src) out)
    cases

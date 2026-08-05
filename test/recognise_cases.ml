open Printf
open Lexparse.Regex

(* One corpus, run against both recognisers. It doubles as a test of [r]: every
   regex here is written in concrete syntax, so a change to the parser in
   regex.ml shows up as a change in what is accepted. *)

(* the whole mlot vocabulary, composed exactly as the recogniser tests had it:
   one token, then any number of whitespace-separated tokens. The parentheses
   in [operators] must be escaped, or they group instead of matching — which
   would make [recognise_one] nullable and let a run of whitespace stand in
   for a token. *)
let keywords = r "let|rec|in|fun|true|false"
let operators = r {|=|\+|->|\(|\)|}
let ident = r "[a-zA-Z][a-zA-Z0-9]*"
let literal = r "-?[0-9]+"
let whitespace = r {|\s|}
let recognise_one = keywords >| operators >| ident >| literal
let mlot = epsilon >| recognise_one >& ~*(whitespace >& recognise_one)

let cases =
  [
    ("a", r "a", [ ""; "a"; "b"; "aa" ]);
    ("ab", r "ab", [ "ab"; "a"; "b"; "abc" ]);
    ("a|b", r "a|b", [ ""; "a"; "b"; "c"; "ab" ]);
    ("a*", r "a*", [ ""; "a"; "aaa"; "b"; "ab" ]);
    ("a+", r "a+", [ ""; "a"; "aaa"; "b" ]);
    ("a?", r "a?", [ ""; "a"; "aa" ]);
    ("(ab)*", r "(ab)*", [ ""; "ab"; "abab"; "aba"; "b" ]);
    ("(a|b)*c", r "(a|b)*c", [ "c"; "ac"; "abbac"; "ab"; "" ]);
    ("[a-c]", r "[a-c]", [ "a"; "b"; "c"; "d"; "" ]);
    ("[a-c]*", r "[a-c]*", [ ""; "abc"; "cba"; "abd" ]);
    (* the pieces the mlot vocabulary is built from *)
    ( "let|rec|in|fun|true|false",
      r "let|rec|in|fun|true|false",
      [ "let"; "rec"; "in"; "fun"; "true"; "false"; "letrec"; "le" ] );
    ( "[a-zA-Z][a-zA-Z0-9]*",
      r "[a-zA-Z][a-zA-Z0-9]*",
      [ "x"; "x1"; "X9y"; ""; "1x"; "x_" ] );
    ("-?[0-9]+", r "-?[0-9]+", [ "0"; "42"; "-42"; ""; "-"; "--1"; "4a" ]);
    ({|\s|}, r {|\s|}, [ " "; "\t"; "\n"; ""; "a"; "  " ]);
    ({|\+|}, r {|\+|}, [ "+"; ""; "++" ]);
    ({|\(|}, r {|\(|}, [ "("; ")"; "" ]);
    ("->", r "->", [ "->"; "-"; ">"; "" ]);
    ({|=|\+|->|\(|\)|}, operators, [ "="; "+"; "->"; "("; ")"; "" ]);
    (* the same alternation with its parentheses left unescaped: they group,
       so [(|)] is [(eps|eps)] — it matches nothing but the empty string *)
    ({|=|\+|->|(|)|}, r {|=|\+|->|(|)|}, [ "="; "+"; "->"; "("; ")"; "" ]);
    (* the composed vocabulary: tokens separated by exactly one whitespace *)
    ( "<mlot>",
      mlot,
      [
        "";
        "fun";
        "x";
        "-123";
        "0123";
        "fun ->";
        "fun->";
        "let x = 1 in x";
        "x  y";
        "@";
      ] );
  ]

let run build recognise =
  List.iter
    (fun (label, rx, inputs) ->
      let m = build rx in
      List.iter
        (fun s ->
          printf "%-26s %-6s %b\n" label (sprintf "%S" s) (recognise m s))
        inputs)
    cases

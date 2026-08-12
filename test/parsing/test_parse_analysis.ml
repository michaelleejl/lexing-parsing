open Lexparse.Lang
open Printf
open Lexparse
open Lexparse.Mlot
open Lexparse.Parsing.Analysis

module Report (Grammar : GRAMMAR) = struct
  module Elaborated = Elaborate (Grammar)
  module A = GrammarAnalysis (Elaborated.Bnf)

  let nt = Grammar.Nonterminal.to_string
  let term = Grammar.Terminal.to_string
  let sym = Grammar.Sym.to_string
  let te = function A.TE.Eps -> "eps" | A.TE.Term t -> term t
  let tset s = A.TSet.elements s |> List.map term |> String.concat " "
  let teset s = A.TESet.elements s |> List.map te |> String.concat " "

  let rhs_str = function
    | [] -> "eps"
    | rhs -> List.map sym rhs |> String.concat " "

  let productions =
    let rec group = function
      | [] -> []
      | (p : Grammar.production) :: ps ->
          let same, rest =
            List.partition (fun (q : Grammar.production) -> q.lhs = p.lhs) ps
          in
          (p.lhs, p :: same) :: group rest
    in
    group (Grammar.productions.start :: Grammar.productions.rest)

  let nullable () =
    A.NTSet.elements A.Nullable.set
    |> List.map nt |> String.concat " " |> printf "nullable: %s\n"

  let first () =
    A.NTMap.iter
      (fun n ts -> printf "first(%-5s) = %s\n" (nt n) (teset ts))
      A.First.table

  let follow () =
    A.NTMap.iter
      (fun n ts -> printf "follow(%-5s) = %s\n" (nt n) (tset ts))
      A.Follow.table

  let predict_set lhs (p : Grammar.production) =
    let first = A.First.syms p.rhs |> A.drop_eps in
    if A.Nullable.syms p.rhs then A.TSet.union first (A.Follow.nonterminal lhs)
    else first

  let predict () =
    List.iter
      (fun (lhs, rhss) ->
        List.iter
          (fun (p : Grammar.production) ->
            printf "%-5s ::= %-24s { %s }\n" (nt lhs) (rhs_str p.rhs)
              (tset (predict_set lhs p)))
          rhss)
      productions

  let conflicts (lhs, rhss) =
    let rec pairwise = function
      | [] -> []
      | s :: rest ->
          List.filter_map
            (fun s' ->
              let common = A.TSet.inter s s' in
              if A.TSet.is_empty common then None else Some (lhs, common))
            rest
          @ pairwise rest
    in
    pairwise (List.map (predict_set lhs) rhss)

  let show_first syms = A.First.syms syms |> teset |> printf "%s"
  let show_nullable syms = A.Nullable.syms syms |> printf "%b"

  let ll1 () =
    match List.concat_map conflicts productions with
    | [] -> printf "LL(1): yes\n"
    | cs ->
        printf "LL(1): no\n";
        List.iter
          (fun (lhs, common) ->
            printf "  %s conflicts on %s\n" (nt lhs) (tset common))
          cs
end

module Factored = Report (Grammars.LeftFactored)
module Unfactored = Report (Grammars.General)

let%expect_test "left-factored: nullable" =
  Factored.nullable ();
  [%expect {| nullable: T' F' G' |}]

let%expect_test "left-factored: first" =
  Factored.first ();
  [%expect
    {|
    first(Start) = IDENT NUM TRUE FALSE FUN LPAREN LET
    first(E    ) = IDENT NUM TRUE FALSE FUN LPAREN LET
    first(E'   ) = IDENT REC
    first(T'   ) = EQUALS eps
    first(T    ) = IDENT NUM TRUE FALSE LPAREN
    first(F'   ) = PLUS eps
    first(F    ) = IDENT NUM TRUE FALSE LPAREN
    first(G'   ) = IDENT NUM TRUE FALSE LPAREN eps
    first(G    ) = IDENT NUM TRUE FALSE LPAREN
    first(S    ) = IDENT NUM TRUE FALSE LPAREN
    |}]

let%expect_test "left-factored: follow" =
  Factored.follow ();
  [%expect
    {|
    follow(Start) =
    follow(E    ) = RPAREN IN EOF
    follow(E'   ) = RPAREN IN EOF
    follow(T'   ) = RPAREN IN EOF
    follow(T    ) = RPAREN IN EOF
    follow(F'   ) = RPAREN EQUALS IN EOF
    follow(F    ) = RPAREN EQUALS IN EOF
    follow(G'   ) = RPAREN PLUS EQUALS IN EOF
    follow(G    ) = RPAREN PLUS EQUALS IN EOF
    follow(S    ) = IDENT NUM TRUE FALSE LPAREN RPAREN PLUS EQUALS IN EOF
    |}]

let%expect_test "left-factored: predict sets" =
  Factored.predict ();
  [%expect
    {|
    Start ::= E EOF                    { IDENT NUM TRUE FALSE FUN LPAREN LET }
    E     ::= FUN IDENT ARROW E        { FUN }
    E     ::= LET E'                   { LET }
    E     ::= T                        { IDENT NUM TRUE FALSE LPAREN }
    E'    ::= IDENT EQUALS E IN E      { IDENT }
    E'    ::= REC IDENT EQUALS E IN E  { REC }
    T     ::= F T'                     { IDENT NUM TRUE FALSE LPAREN }
    T'    ::= EQUALS F T'              { EQUALS }
    T'    ::= eps                      { RPAREN IN EOF }
    F     ::= G F'                     { IDENT NUM TRUE FALSE LPAREN }
    F'    ::= PLUS G F'                { PLUS }
    F'    ::= eps                      { RPAREN EQUALS IN EOF }
    G     ::= S G'                     { IDENT NUM TRUE FALSE LPAREN }
    G'    ::= S G'                     { IDENT NUM TRUE FALSE LPAREN }
    G'    ::= eps                      { RPAREN PLUS EQUALS IN EOF }
    S     ::= IDENT                    { IDENT }
    S     ::= NUM                      { NUM }
    S     ::= TRUE                     { TRUE }
    S     ::= FALSE                    { FALSE }
    S     ::= LPAREN E RPAREN          { LPAREN }
    |}]

let%expect_test "left-factored: is LL(1)" =
  Factored.ll1 ();
  [%expect {| LL(1): yes |}]

let%expect_test "general: nullable" =
  Unfactored.nullable ();
  [%expect {| nullable: T' F' G' |}]

let%expect_test "general: first" =
  Unfactored.first ();
  [%expect
    {|
    first(Start) = IDENT NUM TRUE FALSE FUN LPAREN LET
    first(E    ) = IDENT NUM TRUE FALSE FUN LPAREN LET
    first(T'   ) = EQUALS eps
    first(T    ) = IDENT NUM TRUE FALSE LPAREN
    first(F'   ) = PLUS eps
    first(F    ) = IDENT NUM TRUE FALSE LPAREN
    first(G'   ) = IDENT NUM TRUE FALSE LPAREN eps
    first(G    ) = IDENT NUM TRUE FALSE LPAREN
    first(S    ) = IDENT NUM TRUE FALSE LPAREN
    |}]

let%expect_test "general: follow" =
  Unfactored.follow ();
  [%expect
    {|
    follow(Start) =
    follow(E    ) = RPAREN IN EOF
    follow(T'   ) = RPAREN IN EOF
    follow(T    ) = RPAREN IN EOF
    follow(F'   ) = RPAREN EQUALS IN EOF
    follow(F    ) = RPAREN EQUALS IN EOF
    follow(G'   ) = RPAREN PLUS EQUALS IN EOF
    follow(G    ) = RPAREN PLUS EQUALS IN EOF
    follow(S    ) = IDENT NUM TRUE FALSE LPAREN RPAREN PLUS EQUALS IN EOF
    |}]

let%expect_test "general: predict sets" =
  Unfactored.predict ();
  [%expect
    {|
    Start ::= E EOF                    { IDENT NUM TRUE FALSE FUN LPAREN LET }
    E     ::= FUN IDENT ARROW E        { FUN }
    E     ::= LET IDENT EQUALS E IN E  { LET }
    E     ::= LET REC IDENT EQUALS E IN E { LET }
    E     ::= T                        { IDENT NUM TRUE FALSE LPAREN }
    T     ::= F T'                     { IDENT NUM TRUE FALSE LPAREN }
    T'    ::= EQUALS F T'              { EQUALS }
    T'    ::= eps                      { RPAREN IN EOF }
    F     ::= G F'                     { IDENT NUM TRUE FALSE LPAREN }
    F'    ::= PLUS G F'                { PLUS }
    F'    ::= eps                      { RPAREN EQUALS IN EOF }
    G     ::= S G'                     { IDENT NUM TRUE FALSE LPAREN }
    G'    ::= S G'                     { IDENT NUM TRUE FALSE LPAREN }
    G'    ::= eps                      { RPAREN PLUS EQUALS IN EOF }
    S     ::= IDENT                    { IDENT }
    S     ::= NUM                      { NUM }
    S     ::= TRUE                     { TRUE }
    S     ::= FALSE                    { FALSE }
    S     ::= LPAREN E RPAREN          { LPAREN }
    |}]

let%expect_test "general: is LL(1)" =
  Unfactored.ll1 ();
  [%expect {|
    LL(1): no
      E conflicts on LET
    |}]

module F = Grammars.LeftFactored

let show_first = Factored.show_first
let show_nullable = Factored.show_nullable

let%expect_test "first of eps" =
  show_first [];
  [%expect {| eps |}]

let%expect_test "first stops at the first non-nullable symbol" =
  show_first [ F.T PLUS; F.N E ];
  [%expect {| PLUS |}]

let%expect_test "first sees through a nullable prefix" =
  show_first [ F.N G'; F.T EQUALS ];
  [%expect {| IDENT NUM TRUE FALSE LPAREN EQUALS |}]

let%expect_test "first of an all-nullable sequence keeps eps" =
  show_first [ F.N G'; F.N T' ];
  [%expect {| IDENT NUM TRUE FALSE LPAREN EQUALS eps |}]

let%expect_test "nullable: empty sequence" =
  show_nullable [];
  [%expect {| true |}]

let%expect_test "nullable: all-nullable sequence" =
  show_nullable [ F.N G'; F.N T' ];
  [%expect {| true |}]

let%expect_test "nullable: sequence with a terminal" =
  show_nullable [ F.N G'; F.T PLUS ];
  [%expect {| false |}]

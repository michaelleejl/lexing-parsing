open Lexparse.Lang
open Printf
open Lexparse
open Lexparse.Mlot
open Lexparse.Parsing.Analysis

(* Reports on the augmented grammar, so the augmented production [S' ::= E $]
   shows up alongside the grammar's own. *)
module Report (Grammar : GRAMMAR) = struct
  module Augmented = Topdown_augment (Grammar)
  module Bnf = Augmented.Bnf
  module A = Grammar_analysis (Bnf)
  open Views (Bnf)

  let nt = Bnf.Nonterminal.to_string
  let term = Bnf.Terminal.to_string
  let sym = Bnf.Sym.to_string

  let te = function
    | A.Term_or_eps.Eps -> "eps"
    | A.Term_or_eps.Term t -> term t

  let tset s = A.Terminal_set.elements s |> List.map term |> String.concat " "
  let teset s = A.Term_or_eps_set.elements s |> List.map te |> String.concat " "

  let rhs_str = function
    | [] -> "eps"
    | rhs -> List.map sym rhs |> String.concat " "

  let productions = production_rules

  (* Lift a symbol of the source grammar into the augmented one. *)
  let t x : Bnf.sym = Bnf.T x
  let n x : Bnf.sym = Bnf.N (Bnf.Nonterminal.Source x)

  let nullable () =
    A.Nonterminal_set.elements A.Nullable.nonterminals
    |> List.map nt |> String.concat " " |> printf "nullable: %s\n"

  let first () =
    A.Nonterminal_map.iter
      (fun n ts -> printf "first(%-5s) = %s\n" (nt n) (teset ts))
      A.First.table

  let follow () =
    A.Nonterminal_map.iter
      (fun n ts -> printf "follow(%-5s) = %s\n" (nt n) (tset ts))
      A.Follow.table

  let predict_set lhs (p : Bnf.production) =
    let first = A.First.syms p.rhs |> A.to_terminals in
    if A.Nullable.syms p.rhs then
      A.Terminal_set.union first (A.Follow.nonterminal lhs)
    else first

  let predict () =
    List.iter
      (fun (lhs, rhss) ->
        List.iter
          (fun (p : Bnf.production) ->
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
              let common = A.Terminal_set.inter s s' in
              if A.Terminal_set.is_empty common then None else Some (lhs, common))
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

module Ll1 = Report (Grammars.Ll1)
module Non_left_recursive = Report (Grammars.Non_left_recursive)

let%expect_test "ll1: nullable" =
  Ll1.nullable ();
  [%expect {| nullable: T' F' G' |}]

let%expect_test "ll1: first" =
  Ll1.first ();
  [%expect
    {|
    first(S'   ) = IDENT NUM TRUE FALSE FUN LPAREN LET
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

let%expect_test "ll1: follow" =
  Ll1.follow ();
  [%expect
    {|
    follow(S'   ) = EOF
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

let%expect_test "ll1: predict sets" =
  Ll1.predict ();
  [%expect
    {|
    S'    ::= E EOF                    { IDENT NUM TRUE FALSE FUN LPAREN LET }
    E     ::= FUN IDENT ARROW E        { FUN }
    E     ::= LET E'                   { LET }
    E     ::= T                        { IDENT NUM TRUE FALSE LPAREN }
    E'    ::= IDENT EQUALS E IN E      { IDENT }
    E'    ::= REC IDENT EQUALS E IN E  { REC }
    T'    ::= EQUALS F T'              { EQUALS }
    T'    ::= eps                      { RPAREN IN EOF }
    T     ::= F T'                     { IDENT NUM TRUE FALSE LPAREN }
    F'    ::= PLUS G F'                { PLUS }
    F'    ::= eps                      { RPAREN EQUALS IN EOF }
    F     ::= G F'                     { IDENT NUM TRUE FALSE LPAREN }
    G'    ::= S G'                     { IDENT NUM TRUE FALSE LPAREN }
    G'    ::= eps                      { RPAREN PLUS EQUALS IN EOF }
    G     ::= S G'                     { IDENT NUM TRUE FALSE LPAREN }
    S     ::= IDENT                    { IDENT }
    S     ::= NUM                      { NUM }
    S     ::= TRUE                     { TRUE }
    S     ::= FALSE                    { FALSE }
    S     ::= LPAREN E RPAREN          { LPAREN }
    |}]

let%expect_test "ll1: is LL(1)" =
  Ll1.ll1 ();
  [%expect {| LL(1): yes |}]

let%expect_test "non-left-recursive: nullable" =
  Non_left_recursive.nullable ();
  [%expect {| nullable: T' F' G' |}]

let%expect_test "non-left-recursive: first" =
  Non_left_recursive.first ();
  [%expect
    {|
    first(S'   ) = IDENT NUM TRUE FALSE FUN LPAREN LET
    first(E    ) = IDENT NUM TRUE FALSE FUN LPAREN LET
    first(T'   ) = EQUALS eps
    first(T    ) = IDENT NUM TRUE FALSE LPAREN
    first(F'   ) = PLUS eps
    first(F    ) = IDENT NUM TRUE FALSE LPAREN
    first(G'   ) = IDENT NUM TRUE FALSE LPAREN eps
    first(G    ) = IDENT NUM TRUE FALSE LPAREN
    first(S    ) = IDENT NUM TRUE FALSE LPAREN
    |}]

let%expect_test "non-left-recursive: follow" =
  Non_left_recursive.follow ();
  [%expect
    {|
    follow(S'   ) = EOF
    follow(E    ) = RPAREN IN EOF
    follow(T'   ) = RPAREN IN EOF
    follow(T    ) = RPAREN IN EOF
    follow(F'   ) = RPAREN EQUALS IN EOF
    follow(F    ) = RPAREN EQUALS IN EOF
    follow(G'   ) = RPAREN PLUS EQUALS IN EOF
    follow(G    ) = RPAREN PLUS EQUALS IN EOF
    follow(S    ) = IDENT NUM TRUE FALSE LPAREN RPAREN PLUS EQUALS IN EOF
    |}]

let%expect_test "non-left-recursive: predict sets" =
  Non_left_recursive.predict ();
  [%expect
    {|
    S'    ::= E EOF                    { IDENT NUM TRUE FALSE FUN LPAREN LET }
    E     ::= FUN IDENT ARROW E        { FUN }
    E     ::= LET IDENT EQUALS E IN E  { LET }
    E     ::= LET REC IDENT EQUALS E IN E { LET }
    E     ::= T                        { IDENT NUM TRUE FALSE LPAREN }
    T'    ::= EQUALS F T'              { EQUALS }
    T'    ::= eps                      { RPAREN IN EOF }
    T     ::= F T'                     { IDENT NUM TRUE FALSE LPAREN }
    F'    ::= PLUS G F'                { PLUS }
    F'    ::= eps                      { RPAREN EQUALS IN EOF }
    F     ::= G F'                     { IDENT NUM TRUE FALSE LPAREN }
    G'    ::= S G'                     { IDENT NUM TRUE FALSE LPAREN }
    G'    ::= eps                      { RPAREN PLUS EQUALS IN EOF }
    G     ::= S G'                     { IDENT NUM TRUE FALSE LPAREN }
    S     ::= IDENT                    { IDENT }
    S     ::= NUM                      { NUM }
    S     ::= TRUE                     { TRUE }
    S     ::= FALSE                    { FALSE }
    S     ::= LPAREN E RPAREN          { LPAREN }
    |}]

let%expect_test "non-left-recursive: is LL(1)" =
  Non_left_recursive.ll1 ();
  [%expect {|
    LL(1): no
      E conflicts on LET
    |}]

module Grammar = Grammars.Ll1

let show_first = Ll1.show_first
let show_nullable = Ll1.show_nullable

let%expect_test "first of eps" =
  show_first [];
  [%expect {| eps |}]

let%expect_test "first stops at the first non-nullable symbol" =
  show_first [ Ll1.t PLUS; Ll1.n Grammar.Nonterminal.E ];
  [%expect {| PLUS |}]

let%expect_test "first sees through a nullable prefix" =
  show_first [ Ll1.n Grammar.Nonterminal.G'; Ll1.t EQUALS ];
  [%expect {| IDENT NUM TRUE FALSE LPAREN EQUALS |}]

let%expect_test "first of an all-nullable sequence keeps eps" =
  show_first [ Ll1.n Grammar.Nonterminal.G'; Ll1.n Grammar.Nonterminal.T' ];
  [%expect {| IDENT NUM TRUE FALSE LPAREN EQUALS eps |}]

let%expect_test "nullable: empty sequence" =
  show_nullable [];
  [%expect {| true |}]

let%expect_test "nullable: all-nullable sequence" =
  show_nullable [ Ll1.n Grammar.Nonterminal.G'; Ll1.n Grammar.Nonterminal.T' ];
  [%expect {| true |}]

let%expect_test "nullable: sequence with a terminal" =
  show_nullable [ Ll1.n Grammar.Nonterminal.G'; Ll1.t PLUS ];
  [%expect {| false |}]

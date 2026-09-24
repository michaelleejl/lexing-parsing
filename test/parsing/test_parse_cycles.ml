open Printf
open Lexparse.Lang
open Lexparse.Mlot
open Lexparse.Parsing.Analysis

(* [Grammar_analysis] runs its cycle check while the functor is applied, so each
   case applies it inside a thunk and catches what comes back out. *)

module Self = Bottomup_augment (Cyclic_grammars.Self_cycle)
module Unit_ = Bottomup_augment (Cyclic_grammars.Unit_cycle)
module Padded = Bottomup_augment (Cyclic_grammars.Padded_cycle)
module Left_rec = Bottomup_augment (Cyclic_grammars.Left_recursive)
module Null_tail = Bottomup_augment (Cyclic_grammars.Nullable_tail)
module Null_prefix = Bottomup_augment (Cyclic_grammars.Nullable_prefix)
module Natural = Bottomup_augment (Grammars.Natural)
module Non_left_recursive = Bottomup_augment (Grammars.Non_left_recursive)
module Ll1 = Bottomup_augment (Grammars.Ll1)

let verdict f =
  try
    f ();
    "acyclic"
  with Cyclic_grammar -> "cyclic"

let case name expected f =
  let actual = verdict f in
  printf "%-18s %-8s %s\n" name actual
    (if actual = expected then "ok" else sprintf "MISMATCH (want %s)" expected)

let%expect_test "cycle detection" =
  (* S ::= S | NUM *)
  case "self cycle" "cyclic" (fun () ->
      let module M = Grammar_analysis (Self.Bnf) in
      ignore M.Cycles.table);
  (* S ::= A | NUM, A ::= S *)
  case "unit cycle" "cyclic" (fun () ->
      let module M = Grammar_analysis (Unit_.Bnf) in
      ignore M.Cycles.table);
  (* S ::= B A C | NUM, A ::= S, B ::= eps, C ::= eps *)
  case "padded cycle" "cyclic" (fun () ->
      let module M = Grammar_analysis (Padded.Bnf) in
      ignore M.Cycles.table);
  (* S ::= S PLUS A | A, A ::= NUM *)
  case "left recursive" "acyclic" (fun () ->
      let module M = Grammar_analysis (Left_rec.Bnf) in
      ignore M.Cycles.table);
  (* S ::= A B, B ::= eps | S, A ::= NUM *)
  case "nullable tail" "acyclic" (fun () ->
      let module M = Grammar_analysis (Null_tail.Bnf) in
      ignore M.Cycles.table);
  (* S ::= A B, A ::= eps, B ::= NUM *)
  case "nullable prefix" "acyclic" (fun () ->
      let module M = Grammar_analysis (Null_prefix.Bnf) in
      ignore M.Cycles.table);
  [%expect
    {|
    self cycle         cyclic   ok
    unit cycle         cyclic   ok
    padded cycle       cyclic   ok
    left recursive     acyclic  ok
    nullable tail      acyclic  ok
    nullable prefix    acyclic  ok
    |}]

(* The grammars the rest of the suite parses with are all acyclic; Natural is
   left-recursive and Ll1 has nullable tails, so between them they cover both
   ways the check has previously been too eager. *)
let%expect_test "shipped grammars are acyclic" =
  case "Natural" "acyclic" (fun () ->
      let module M = Grammar_analysis (Natural.Bnf) in
      ignore M.Cycles.table);
  case "Non_left_recursive" "acyclic" (fun () ->
      let module M = Grammar_analysis (Non_left_recursive.Bnf) in
      ignore M.Cycles.table);
  case "Ll1" "acyclic" (fun () ->
      let module M = Grammar_analysis (Ll1.Bnf) in
      ignore M.Cycles.table);
  [%expect
    {|
    Natural            acyclic  ok
    Non_left_recursive acyclic  ok
    Ll1                acyclic  ok
    |}]

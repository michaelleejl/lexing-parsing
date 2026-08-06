open Printf

module Sym = struct
  type t = S | A | B | Ca | Cb

  let compare = compare
  let to_str = function S -> "S" | A -> "A" | B -> "B" | Ca -> "a" | Cb -> "b"
end

module Tag = struct
  type t = PushASB | PushEps | ReadA | ReadB | SToA | SToB | AToCa | BToCa

  let compare = compare

  let to_str = function
    | PushASB -> "S->aSb"
    | PushEps -> "S->eps"
    | ReadA -> "read a"
    | ReadB -> "read b"
    | SToA -> "S->A"
    | SToB -> "S->B"
    | AToCa -> "A->a"
    | BToCa -> "B->a"
end

module P = Lexparse.Automata.Tpda.Make (Char) (Sym) (Tag)

let transitions rules =
  List.fold_left
    (fun m (key, out) ->
      let existing =
        match P.Transition.find_opt key m with
        | Some s -> s
        | None -> P.TransitionOutputSet.empty
      in
      P.Transition.add key (P.TransitionOutputSet.add out existing) m)
    P.Transition.empty rules

let pda rules start =
  let ts = transitions rules in
  P.
    {
      states = P.State.singleton 0;
      next = (fun _ -> ts);
      initial_state = 0;
      initial_stack_sym = start;
    }

let anbn =
  pda
    [
      ((None, Sym.S), (0, [ Sym.Ca; Sym.S; Sym.Cb ], Tag.PushASB));
      ((None, Sym.S), (0, [], Tag.PushEps));
      ((Some 'a', Sym.Ca), (0, [], Tag.ReadA));
      ((Some 'b', Sym.Cb), (0, [], Tag.ReadB));
    ]
    Sym.S

let ambiguous =
  pda
    [
      ((None, Sym.S), (0, [ Sym.A ], Tag.SToA));
      ((None, Sym.S), (0, [ Sym.B ], Tag.SToB));
      ((None, Sym.A), (0, [ Sym.Ca ], Tag.AToCa));
      ((None, Sym.B), (0, [ Sym.Ca ], Tag.BToCa));
      ((Some 'a', Sym.Ca), (0, [], Tag.ReadA));
    ]
    Sym.S

let initial machine =
  P.TraceSet.singleton
    ( P.Config.
        {
          current_state = machine.P.initial_state;
          stack = [ machine.P.initial_stack_sym ];
        },
      [] )

let cfg_str P.Config.{ current_state; stack } =
  sprintf "q%d [%s]" current_state
    (List.map Sym.to_str stack |> String.concat " ")

let tags_str tags = List.map Tag.to_str tags |> String.concat ", "

let advance machine traces tok =
  P.TraceSet.fold
    (fun (cfg, tags) acc ->
      P.TraceSet.fold
        (fun (cfg', tags') acc -> P.TraceSet.add (cfg', tags @ tags') acc)
        (P.consume machine (P.TraceSet.singleton (cfg, [])) tok)
        acc)
    traces P.TraceSet.empty

let run machine s =
  let cs = List.init (String.length s) (String.get s) in
  List.fold_left (advance machine) (initial machine) cs
  |> P.epsilon_closure machine

let accepting traces =
  P.TraceSet.filter (fun (cfg, _) -> P.is_accepting cfg) traces

let%expect_test "a^n b^n: which strings are accepted" =
  List.iter
    (fun s ->
      printf "%-8s %b\n" (sprintf "%S" s)
        (run anbn s |> accepting |> P.TraceSet.is_empty |> not))
    [ ""; "ab"; "aabb"; "aaabbb"; "a"; "b"; "ba"; "abb"; "aab"; "abab" ];
  [%expect
    {|
    ""       true
    "ab"     true
    "aabb"   true
    "aaabbb" true
    "a"      false
    "b"      false
    "ba"     false
    "abb"    false
    "aab"    false
    "abab"   false
    |}]

let%expect_test "epsilon closure expands the top nonterminal" =
  P.epsilon_closure anbn (initial anbn)
  |> P.TraceSet.elements
  |> List.iter (fun (cfg, tags) ->
      printf "%-12s %s\n" (cfg_str cfg) (tags_str tags));
  [%expect {|
    q0 []        S->eps
    q0 [S]
    q0 [a S b]   S->aSb
    |}]

let%expect_test "the tags of an accepting run spell out the derivation" =
  run anbn "aabb" |> accepting |> P.TraceSet.elements
  |> List.iter (fun (cfg, tags) ->
      printf "%-6s %s\n" (cfg_str cfg) (tags_str tags));
  [%expect {| q0 []  S->aSb, read a, S->aSb, read a, S->eps, read b, read b |}]

let%expect_test "a rejected string is left stuck with symbols on the stack" =
  let traces = run anbn "aab" in
  P.TraceSet.elements traces
  |> List.iter (fun (cfg, _) ->
      printf "%-8s accepting=%b\n" (cfg_str cfg) (P.is_accepting cfg));
  [%expect {| q0 [b]   accepting=false |}]

let%expect_test "an ambiguous machine keeps both derivations" =
  run ambiguous "a" |> accepting |> P.TraceSet.elements
  |> List.iter (fun (cfg, tags) ->
      printf "%-6s %s\n" (cfg_str cfg) (tags_str tags));
  [%expect
    {|
    q0 []  S->A, A->a, read a
    q0 []  S->B, B->a, read a
    |}]

let%expect_test "consuming past the end of the stack yields nothing" =
  let empty_stack =
    P.TraceSet.singleton (P.Config.{ current_state = 0; stack = [] }, [])
  in
  printf "%d\n" (P.TraceSet.cardinal (P.consume anbn empty_stack 'a'));
  [%expect {| 0 |}]

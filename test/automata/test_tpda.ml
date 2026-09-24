open Printf

module Sym = struct
  type t = S | A | B | Ca | Cb

  let compare = compare
  let to_string = function S -> "S" | A -> "A" | B -> "B" | Ca -> "a" | Cb -> "b"
end

module Tag = struct
  type t = Push_asb | Push_eps | Read_a | Read_b | S_to_a | S_to_b | A_to_ca | B_to_ca

  let compare = compare

  let to_string = function
    | Push_asb -> "S->aSb"
    | Push_eps -> "S->eps"
    | Read_a -> "read a"
    | Read_b -> "read b"
    | S_to_a -> "S->A"
    | S_to_b -> "S->B"
    | A_to_ca -> "A->a"
    | B_to_ca -> "B->a"
end

module P = Lexparse.Automata.Tpda.Make (Char) (Sym) (Tag)

let transitions rules =
  List.fold_left
    (fun m (key, out) ->
      let existing =
        match P.Transition_map.find_opt key m with
        | Some s -> s
        | None -> P.Transition_output_set.empty
      in
      P.Transition_map.add key (P.Transition_output_set.add out existing) m)
    P.Transition_map.empty rules

let pda rules start =
  let ts = transitions rules in
  P.
    {
      states = P.State_set.singleton 0;
      next = (fun _ -> ts);
      initial_state = 0;
      initial_stack_sym = start;
    }

let anbn =
  pda
    [
      ((None, Sym.S), (0, [ Sym.Ca; Sym.S; Sym.Cb ], Tag.Push_asb));
      ((None, Sym.S), (0, [], Tag.Push_eps));
      ((Some 'a', Sym.Ca), (0, [], Tag.Read_a));
      ((Some 'b', Sym.Cb), (0, [], Tag.Read_b));
    ]
    Sym.S

let ambiguous =
  pda
    [
      ((None, Sym.S), (0, [ Sym.A ], Tag.S_to_a));
      ((None, Sym.S), (0, [ Sym.B ], Tag.S_to_b));
      ((None, Sym.A), (0, [ Sym.Ca ], Tag.A_to_ca));
      ((None, Sym.B), (0, [ Sym.Ca ], Tag.B_to_ca));
      ((Some 'a', Sym.Ca), (0, [], Tag.Read_a));
    ]
    Sym.S

let initial machine =
  P.Trace_set.singleton
    ( P.Config.
        {
          current_state = machine.P.initial_state;
          stack = [ machine.P.initial_stack_sym ];
        },
      [] )

let cfg_str P.Config.{ current_state; stack } =
  sprintf "q%d [%s]" current_state
    (List.map Sym.to_string stack |> String.concat " ")

let tags_str tags = List.map Tag.to_string tags |> String.concat ", "

let advance machine traces tok =
  P.Trace_set.fold
    (fun (cfg, tags) acc ->
      P.Trace_set.fold
        (fun (cfg', tags') acc -> P.Trace_set.add (cfg', tags @ tags') acc)
        (P.consume machine (P.Trace_set.singleton (cfg, [])) tok)
        acc)
    traces P.Trace_set.empty

let run machine s =
  let cs = List.init (String.length s) (String.get s) in
  List.fold_left (advance machine) (initial machine) cs
  |> P.epsilon_closure machine

let accepting traces =
  P.Trace_set.filter (fun (cfg, _) -> P.is_accepting cfg) traces

let%expect_test "a^n b^n: which strings are accepted" =
  List.iter
    (fun s ->
      printf "%-8s %b\n" (sprintf "%S" s)
        (run anbn s |> accepting |> P.Trace_set.is_empty |> not))
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
  |> P.Trace_set.elements
  |> List.iter (fun (cfg, tags) ->
      printf "%-12s %s\n" (cfg_str cfg) (tags_str tags));
  [%expect {|
    q0 []        S->eps
    q0 [S]
    q0 [a S b]   S->aSb
    |}]

let%expect_test "the tags of an accepting run spell out the derivation" =
  run anbn "aabb" |> accepting |> P.Trace_set.elements
  |> List.iter (fun (cfg, tags) ->
      printf "%-6s %s\n" (cfg_str cfg) (tags_str tags));
  [%expect {| q0 []  S->aSb, read a, S->aSb, read a, S->eps, read b, read b |}]

let%expect_test "a rejected string is left stuck with symbols on the stack" =
  let traces = run anbn "aab" in
  P.Trace_set.elements traces
  |> List.iter (fun (cfg, _) ->
      printf "%-8s accepting=%b\n" (cfg_str cfg) (P.is_accepting cfg));
  [%expect {| q0 [b]   accepting=false |}]

let%expect_test "an ambiguous machine keeps both derivations" =
  run ambiguous "a" |> accepting |> P.Trace_set.elements
  |> List.iter (fun (cfg, tags) ->
      printf "%-6s %s\n" (cfg_str cfg) (tags_str tags));
  [%expect
    {|
    q0 []  S->A, A->a, read a
    q0 []  S->B, B->a, read a
    |}]

let%expect_test "consuming past the end of the stack yields nothing" =
  let empty_stack =
    P.Trace_set.singleton (P.Config.{ current_state = 0; stack = [] }, [])
  in
  printf "%d\n" (P.Trace_set.cardinal (P.consume anbn empty_stack 'a'));
  [%expect {| 0 |}]

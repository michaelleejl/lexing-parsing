open Compilers.Intfs.Tags
open Compilers.Tnfa
open Compilers.Mlot
open Compilers.Regex
open Mlot_Token
open Printf

module Tag = struct
  type t =
    | T_TRUE
    | T_FALSE
    | T_FUN
    | T_ARROW
    | T_LPARAN
    | T_RPARAN
    | T_PLUS
    | T_LET
    | T_EQUALS
    | T_IN
    | T_REC
    | T_IDENT
    | T_NUM
    | T_SKIP

  type token = Mlot_Token.t

  let compare = compare

  let tag_to_action = function
    | T_TRUE -> fun _ -> Some TRUE
    | T_FALSE -> fun _ -> Some FALSE
    | T_FUN -> fun _ -> Some FUN
    | T_ARROW -> fun _ -> Some ARROW
    | T_LPARAN -> fun _ -> Some LPARAN
    | T_RPARAN -> fun _ -> Some RPARAN
    | T_PLUS -> fun _ -> Some PLUS
    | T_LET -> fun _ -> Some LET
    | T_EQUALS -> fun _ -> Some EQUALS
    | T_IN -> fun _ -> Some IN
    | T_REC -> fun _ -> Some REC
    | T_IDENT -> fun cs -> Some (IDENT (Base.String.of_list cs))
    | T_NUM ->
        fun cs -> Some (NUM (cs |> Base.String.of_list |> Base.Int.of_string))
    | T_SKIP -> fun _ -> None
end

module TaggedNfa = Make (Tag)
open TaggedNfa
open Tag

let run tnfa s =
  List.fold_left
    (fun qs c -> step tnfa qs c)
    (initialise tnfa) (Base.String.to_list s)

let describe_tag = function
  | T_TRUE -> "TRUE"
  | T_FALSE -> "FALSE"
  | T_FUN -> "FUN"
  | T_ARROW -> "ARROW"
  | T_LPARAN -> "LPARAN"
  | T_RPARAN -> "RPARAN"
  | T_PLUS -> "PLUS"
  | T_LET -> "LET"
  | T_EQUALS -> "EQUALS"
  | T_IN -> "IN"
  | T_REC -> "REC"
  | T_IDENT -> "IDENT"
  | T_NUM -> "NUM"
  | T_SKIP -> "SKIP"

let describe_tag_opt = function None -> "None" | Some t -> describe_tag t

let%expect_test "lift: single char accepts and emits tag" =
  let tn = lift (compile (chr 'a')) T_IDENT in
  printf "%b %s"
    (is_accepting tn (run tn "a"))
    (describe_tag_opt (emit_tag tn (run tn "a")));
  [%expect {| true IDENT |}]

let%expect_test "lift: wrong char ends in empty set" =
  let tn = lift (compile (chr 'a')) T_LET in
  let qs = run tn "b" in
  printf "%b %s" (is_rejecting tn qs) (describe_tag_opt (emit_tag tn qs));
  [%expect {| true None |}]

let%expect_test "alt: branches tagged distinctly" =
  let tn =
    alt (lift (compile (chr 'a')) T_IDENT) (lift (compile (str "let")) T_LET)
  in
  printf "%s / %s"
    (describe_tag_opt (emit_tag tn (run tn "a")))
    (describe_tag_opt (emit_tag tn (run tn "let")));
  [%expect {| IDENT / LET |}]

let%expect_test "alt: overlapping patterns keep larger tag" =
  let tn =
    alt
      (lift (compile (parse "a*let")) T_IDENT)
      (lift (compile (str "let")) T_LET)
  in
  printf "%s" (describe_tag_opt (emit_tag tn (run tn "let")));
  [%expect {| LET |}]

let%expect_test "keyword vs ident: keyword tag wins on fun" =
  let tn =
    alt
      (lift (compile (parse "fun")) T_FUN)
      (lift (compile (parse "[a-z]+")) T_IDENT)
  in
  printf "%s" (describe_tag_opt (emit_tag tn (run tn "fun")));
  [%expect {| FUN |}]

let%expect_test "keyword vs ident: ident for non-keyword letters" =
  let tn =
    alt
      (lift (compile (parse "fun")) T_FUN)
      (lift (compile (parse "[a-z]+")) T_IDENT)
  in
  printf "%s" (describe_tag_opt (emit_tag tn (run tn "xyz")));
  [%expect {| IDENT |}]

let%expect_test "whitespace emits SKIP" =
  let tn = lift (compile (parse {|\s*|})) T_SKIP in
  printf "%b %s"
    (is_accepting tn (run tn "  "))
    (describe_tag_opt (emit_tag tn (run tn "  ")));
  [%expect {| true SKIP |}]

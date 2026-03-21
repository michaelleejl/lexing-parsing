module C = Set.Make (Char)

type 'c rgx =
  | Empty
  | Epsilon
  | Char of 'c
  | Alt of 'c rgx * 'c rgx
  | Seq of 'c rgx * 'c rgx
  | Kleene of 'c rgx

let rec compile r =
  match r with
  | Empty -> Nfa.empty
  | Epsilon -> Nfa.epsilon
  | Char cs -> Nfa.one_of (C.to_list cs)
  | Alt (r1, r2) -> Nfa.alt (compile r1) (compile r2)
  | Seq (r1, r2) -> Nfa.seq (compile r1) (compile r2)
  | Kleene r -> Nfa.kleene (compile r)

type t = C.t rgx

let empty = Empty
let epsilon = Epsilon
let chr c = Char (C.singleton c)

let str s =
  List.fold_right
    (fun c -> fun acc -> Seq (Char (C.singleton c), acc))
    (Base.String.to_list s) Epsilon

let chrs cs = Char cs

let alt r1 r2 =
  match (r1, r2) with
  | Char c1, Char c2 -> Char (C.union c1 c2)
  | r1, r2 -> Alt (r1, r2)

let seq r1 r2 = Seq (r1, r2)
let kleene r = Kleene r
let plus r = Seq (r, Kleene r)
let opt r = Alt (Epsilon, r)

let range_ l h =
  let rec loop i h acc =
    if i = h then C.add (Char.chr i) acc
    else loop (i + 1) h (C.add (Char.chr i) acc)
  in
  loop l h C.empty

let range l h = Char (range_ l h)
let any_ = range_ 0 255
let any = Char any_

module Parse = struct
  exception Failure

  module Bracket = struct
    type elt = Char of char | Range of char * char
    type t = { negated : bool; elements : elt list }

    let interpret { negated; elements } =
      let cs =
        List.fold_right
          (function
            | Char c -> C.add c
            | Range (cl, ch) -> C.union (range_ (Char.code cl) (Char.code ch)))
          elements C.empty
      in
      if negated then C.diff any_ cs else cs

    let parse_element cs =
      match cs with
      | [] -> raise Failure
      | ']' :: s -> (None, s)
      | c :: ('-' :: ']' :: _ as s) -> (Some (Char c), s)
      | c :: '-' :: c' :: s -> (Some (Range (c, c')), s)
      | c :: s -> (Some (Char c), s)

    let parse_initial cs =
      match cs with
      | [] -> raise Failure
      | c :: ('-' :: ']' :: _ as s) -> (Some (Char c), s)
      | c :: '-' :: c' :: s -> (Some (Range (c, c')), s)
      | c :: s -> (Some (Char c), s)

    let parse cs =
      let rec loop elts s =
        match parse_element s with
        | None, s' -> (List.rev elts, s')
        | Some e, s' -> loop (e :: elts) s'
      in
      match parse_initial cs with
      | None, s -> ([], s)
      | Some e, s -> loop [ e ] s
  end

  type t =
    | Empty
    | Epsilon
    | Char of C.t
    | Alt of t * t
    | Seq of t * t
    | Kleene of t
    | Plus of t
    | Opt of t
    | Any
    | Bracketed of Bracket.t

  let whitespace = C.of_list Char.[ chr 32; chr 12; chr 10; chr 13; chr 9 ]

  let parse_bracketed s =
    match s with
    | '^' :: s' ->
        let elements, rest = Bracket.parse s' in
        (Bracketed { negated = true; elements }, rest)
    | _ :: _ ->
        let elements, rest = Bracket.parse s in
        (Bracketed { negated = false; elements }, rest)
    | [] -> raise Failure

  let rec parse_atom s =
    match s with
    | '(' :: rest -> begin
        match parse_alt rest with
        | r, ')' :: rest' -> Some (r, rest')
        | r, rest' -> None
      end
    | '[' :: rest -> Some (parse_bracketed rest)
    | [] | (')' | '|' | '*' | '+' | '?') :: _ -> None
    | '.' :: cs -> Some (Any, cs)
    | '\\' :: 's' :: cs -> Some (Char whitespace, cs)
    | '\\' :: '+' :: cs -> Some (Char (C.singleton '+'), cs)
    | '\\' :: c :: cs -> raise Failure
    | c :: cs -> Some (Char (C.singleton c), cs)

  and parse_suffixed s =
    match parse_atom s with
    | None -> None
    | Some (r, '*' :: rest) -> Some (Kleene r, rest)
    | Some (r, '?' :: rest) -> Some (Opt r, rest)
    | Some (r, '+' :: rest) -> Some (Plus r, rest)
    | Some (r, rest) -> Some (r, rest)

  and parse_seq s =
    match parse_suffixed s with
    | None -> (Epsilon, s)
    | Some (r, rest) ->
        let r', rest' = parse_seq rest in
        (Seq (r, r'), rest')

  and parse_alt s =
    match parse_seq s with
    | r, '|' :: rest ->
        let r', rest' = parse_alt rest in
        (Alt (r, r'), rest')
    | r, rest -> (r, rest)

  let parse cs = match parse_alt cs with r, [] -> r | r, _ -> raise Failure

  let rec to_alt = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: y :: xs -> x :: '|' :: y :: '|' :: to_alt xs

  let rec interpret = function
    | Empty -> empty
    | Epsilon -> epsilon
    | Char cs -> chrs cs
    | Alt (r1, r2) -> alt (interpret r1) (interpret r2)
    | Seq (r1, r2) -> seq (interpret r1) (interpret r2)
    | Kleene r -> kleene (interpret r)
    | Plus r -> plus (interpret r)
    | Opt r -> opt (interpret r)
    | Any -> any
    | Bracketed b -> Char (Bracket.interpret b)
end

let parse s = Parse.(interpret (parse (Base.String.to_list s)))

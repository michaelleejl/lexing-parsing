open Regex
open Token

type input = char
type token = Token.t
type spec = Regex.t
type action = input list -> token option

let vocabulary =
  [
    (r "let", fun _ -> Some LET);
    (r "rec", fun _ -> Some REC);
    (r "in", fun _ -> Some IN);
    (r "fun", fun _ -> Some FUN);
    (r "true", fun _ -> Some TRUE);
    (r "false", fun _ -> Some FALSE);
    (r "=", fun _ -> Some EQUALS);
    (r {|\+|}, fun _ -> Some PLUS);
    (r "->", fun _ -> Some ARROW);
    (r {|\(|}, fun _ -> Some LPAREN);
    (r {|\)|}, fun _ -> Some RPAREN);
    (r "[a-zA-Z][a-zA-Z0-9]*", fun cs -> Some (IDENT (Base.String.of_list cs)));
    ( r "-?[0-9]+",
      fun cs -> Some (NUM (cs |> Base.String.of_list |> Base.Int.of_string)) );
    (r {|\s|}, fun _ -> None);
  ]

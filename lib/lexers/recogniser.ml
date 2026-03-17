open Intfs
open Base

type result = Failure | PartialMatch of char list
type recogniser = char list -> result

let pattern_match f cs =
  match cs with
  | [] -> Failure
  | c :: cs -> if f c then PartialMatch cs else Failure

let alphabetic = pattern_match Char.is_alpha
let numeric = pattern_match Char.is_digit
let alphanumeric = pattern_match Char.is_alphanum
let whitespace = pattern_match Char.is_whitespace
let chr x = pattern_match (Char.equal x)
let empty _ = Failure
let epsilon cs = PartialMatch cs

let seq (r1 : recogniser) (r2 : recogniser) cs =
  match r1 cs with Failure -> Failure | PartialMatch cs -> r2 cs

let alt (r1 : recogniser) (r2 : recogniser) cs =
  match (r1 cs, r2 cs) with
  | Failure, Failure -> Failure
  | PartialMatch xs, Failure -> PartialMatch xs
  | Failure, PartialMatch ys -> PartialMatch ys
  | PartialMatch xs, PartialMatch ys ->
      if List.length xs <= List.length ys then PartialMatch xs
      else PartialMatch ys

let rec kleene (r : recogniser) cs =
  match r cs with
  | Failure -> PartialMatch cs
  | PartialMatch xs -> kleene r xs

let ( >>& ) = seq
let ( >>| ) = alt
let ( ~~* ) = kleene
let plus (r : recogniser) = r >>& ~~*r
let ( ~~+ ) = plus
let maybe (r : recogniser) = r >>| epsilon
let ( ~~? ) = maybe

let str s =
  let cs' = String.to_list s in
  List.fold cs' ~init:epsilon ~f:(fun acc -> fun x -> acc >>& chr x)

let keywords = [ "let"; "rec"; "in"; "fun"; "true"; "false" ]
let operators = [ "="; "+"; "->"; "("; ")" ]
let ident = alphabetic >>& ~~*alphanumeric
let literal = ~~?(str "-") >>& ~~+numeric

let to_recogniser xs =
  List.fold xs ~init:empty ~f:(fun acc -> fun x -> acc >>| str x)

let recognise_one =
  to_recogniser keywords >>| to_recogniser operators >>| ident >>| literal
  >>| whitespace

let recognise_many =
  epsilon >>| recognise_one >>& ~~*(whitespace >>& recognise_one)

let recognise s =
  match recognise_many (String.to_list s) with
  | PartialMatch [] -> true
  | _ -> false

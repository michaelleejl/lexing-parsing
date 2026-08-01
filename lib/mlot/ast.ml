open Format

type fparam = string

and node =
  | Var of string
  | Num of int
  | Bool of bool
  | Fun of fparam * node
  | App of node * node
  | Let of fparam * node * node
  | LetRec of fparam * node * node
  | Plus of node * node
  | Equals of node * node

let rec to_str t =
  match t with
  | Var x -> sprintf "%s" x
  | Num n -> sprintf "%d" n
  | Bool b -> sprintf "%b" b
  | Fun (x, e) -> sprintf "Fun(%s, %s)" x (to_str e)
  | App (e1, e2) -> sprintf "App(%s, %s)" (to_str e1) (to_str e2)
  | Let (x, e1, e2) -> sprintf "Let(%s, %s, %s)" x (to_str e1) (to_str e2)
  | LetRec (x, e1, e2) ->
      sprintf "LetRec(%s, %s, %s)" x (to_str e1) (to_str e2)
  | Plus (e1, e2) -> sprintf "Plus(%s, %s)" (to_str e1) (to_str e2)
  | Equals (e1, e2) -> sprintf "Equals(%s, %s)" (to_str e1) (to_str e2)

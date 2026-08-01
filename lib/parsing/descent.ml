open Mlot.Token
open Mlot.Ast

type token = Mlot.Token.t
type ast = Mlot.Ast.node

exception ParseFail of string

let rec e = function
  | FUN :: IDENT x :: ARROW :: toks ->
      let body, toks' = e toks in
      (Fun (x, body), toks')
  | LET :: IDENT x :: toks -> begin
      match e toks with
      | arg, IN :: toks' ->
          let body, toks'' = e toks' in
          (Let (x, arg, body), toks'')
      | _ -> raise (ParseFail "LET")
    end
  | LET :: REC :: IDENT x :: toks -> begin
      match e toks with
      | arg, IN :: toks' ->
          let body, toks'' = e toks' in
          (LetRec (x, arg, body), toks'')
      | _ -> raise (ParseFail "LETREC")
    end
  | toks -> t toks

and t' = function
  | EQUALS :: toks ->
      let term, toks' = f toks in
      let terms, toks'' = t' toks' in
      (term :: terms, toks'')
  | toks -> ([], toks)

and t toks =
  let term, toks' = f toks in
  let terms, toks'' = t' toks' in
  (List.fold_left (fun acc term -> Equals (acc, term)) term terms, toks'')

and f' = function
  | PLUS :: toks ->
      let term, toks' = g toks in
      let terms, toks'' = f' toks' in
      (term :: terms, toks'')
  | toks -> ([], toks)

and f toks =
  let term, toks' = g toks in
  let terms, toks'' = f' toks' in
  (List.fold_left (fun acc term -> Plus (acc, term)) term terms, toks'')

and g' toks =
  match s toks with
  | term', toks' ->
      let terms, toks'' = g' toks' in
      (term' :: terms, toks'')
  | exception ParseFail _ -> ([], toks)

and g toks =
  let term, toks' = s toks in
  let terms, toks'' = g' toks' in
  (List.fold_left (fun acc term -> App (acc, term)) term terms, toks'')

and s = function
  | IDENT x :: toks -> (Var x, toks)
  | NUM n :: toks -> (Num n, toks)
  | TRUE :: toks -> (Bool true, toks)
  | FALSE :: toks -> (Bool false, toks)
  | LPAREN :: toks -> begin
      match e toks with
      | term, RPAREN :: toks' -> (term, toks')
      | _ -> raise (ParseFail "S LPAREN")
    end
  | _ -> raise (ParseFail "S")

let parse ts =
  match e ts with expr, [] -> expr | _ -> raise (ParseFail "failed")

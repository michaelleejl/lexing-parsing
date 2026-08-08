open Mlot.Token
open Mlot.Ast

exception ParseFail of string

module General = struct
  type token = Mlot.Token.t
  type ast = Mlot.Ast.node

  let rec e = function
    | FUN :: IDENT x :: ARROW :: toks ->
        let body, toks' = e toks in
        (Fun (x, body), toks')
    | LET :: IDENT x :: EQUALS :: toks ->
        begin match e toks with
        | arg, IN :: toks' ->
            let body, toks'' = e toks' in
            (Let (x, arg, body), toks'')
        | _ -> raise (ParseFail "LET")
        end
    | LET :: REC :: IDENT x :: EQUALS :: toks ->
        begin match e toks with
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
    | LPAREN :: toks ->
        begin match e toks with
        | term, RPAREN :: toks' -> (term, toks')
        | _ -> raise (ParseFail "S LPAREN")
        end
    | _ -> raise (ParseFail "S")

  let parse ts =
    match e ts with expr, [] -> expr | _ -> raise (ParseFail "failed")
end

module LL1 = struct
  type token = Mlot.Token.t
  type ast = Mlot.Ast.node

  let pop = function
    | [] -> raise (ParseFail "cannot pop from empty list")
    | _ :: xs -> xs

  let peek = function [] -> None | x :: _ -> Some x

  (* consume a terminal the prediction has already committed to *)
  let expect tok = function
    | t :: rest when t = tok -> rest
    | _ -> raise (ParseFail ("expected " ^ Mlot.Token.to_str tok))

  let expect_ident = function
    | IDENT x :: rest -> (x, rest)
    | _ -> raise (ParseFail "expected an identifier")

  let rec e toks =
    match peek toks with
    | Some FUN ->
        let toks = expect FUN toks in
        let x, toks = expect_ident toks in
        let toks = expect ARROW toks in
        let body, toks = e toks in
        (Fun (x, body), toks)
    | Some LET -> e' (expect LET toks)
    | _ -> t toks

  and e' toks =
    match peek toks with
    | Some (IDENT _) ->
        let x, toks = expect_ident toks in
        let toks = expect EQUALS toks in
        let arg, toks = e toks in
        let toks = expect IN toks in
        let body, toks = e toks in
        (Let (x, arg, body), toks)
    | Some REC ->
        let toks = expect REC toks in
        let x, toks = expect_ident toks in
        let toks = expect EQUALS toks in
        let arg, toks = e toks in
        let toks = expect IN toks in
        let body, toks = e toks in
        (LetRec (x, arg, body), toks)
    | _ -> raise (ParseFail "E'")

  and t' toks =
    match peek toks with
    | Some EQUALS ->
        let toks = expect EQUALS toks in
        let term, toks = f toks in
        let terms, toks = t' toks in
        (term :: terms, toks)
    | _ -> ([], toks)

  and t toks =
    let term, toks = f toks in
    let terms, toks = t' toks in
    (List.fold_left (fun acc term -> Equals (acc, term)) term terms, toks)

  and f' toks =
    match peek toks with
    | Some PLUS ->
        let toks = expect PLUS toks in
        let term, toks = g toks in
        let terms, toks = f' toks in
        (term :: terms, toks)
    | _ -> ([], toks)

  and f toks =
    let term, toks = g toks in
    let terms, toks = f' toks in
    (List.fold_left (fun acc term -> Plus (acc, term)) term terms, toks)

  and g' toks =
    match peek toks with
    | Some (IDENT _ | NUM _ | TRUE | FALSE | LPAREN) ->
        let term, toks = s toks in
        let terms, toks = g' toks in
        (term :: terms, toks)
    | _ -> ([], toks)

  and g toks =
    let term, toks = s toks in
    let terms, toks = g' toks in
    (List.fold_left (fun acc term -> App (acc, term)) term terms, toks)

  and s toks =
    match peek toks with
    | Some (IDENT x) -> (Var x, pop toks)
    | Some (NUM n) -> (Num n, pop toks)
    | Some TRUE -> (Bool true, pop toks)
    | Some FALSE -> (Bool false, pop toks)
    | Some LPAREN ->
        let toks = expect LPAREN toks in
        let term, toks = e toks in
        let toks = expect RPAREN toks in
        (term, toks)
    | _ -> raise (ParseFail "S")

  let parse ts =
    match e ts with expr, [] -> expr | _ -> raise (ParseFail "failed")
end

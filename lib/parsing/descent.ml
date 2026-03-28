
open Mlot
open Mlot_Token
open Mlot_Ast 

type token = Mlot_Token.t
type ast = Mlot_Ast.node

exception ParseFail of string;;

module Recogniser = struct 
  let rec e = function 
    | FUN :: IDENT _ :: ARROW :: toks -> e toks 
    | LET :: IDENT _ :: toks -> 
        begin match e toks with 
          | IN :: toks' -> e toks' 
          | _ -> raise (ParseFail "LET") end 
    | LET :: REC :: IDENT _ :: toks -> 
        begin match e toks with 
          | IN :: toks' -> e toks' 
          | _ -> raise (ParseFail "LET REC") end 
    | toks -> t toks 

  and t' = function 
    | EQUALS :: toks -> t' (f toks)
    | toks ->  toks 

  and t toks = t' (f toks) 

  and f' = function 
    | PLUS :: toks -> f' (g toks)
    | toks ->  toks 

  and f toks = f' (s toks)

  and g' toks = begin match s toks with
      | toks' -> g' toks' 
      | exception ParseFail _ -> toks end 
  
  and g toks = g' (s toks)
    
  and s = function
    | IDENT _ :: toks -> toks 
    | NUM _ :: toks -> toks 
    | TRUE :: toks -> toks 
    | FALSE :: toks -> toks 
    | LPAREN :: toks -> 
      begin match e toks with 
          | RPAREN :: toks' -> toks'  
          | _ -> raise (ParseFail "S LPAREN") end 
    | _ -> raise (ParseFail "S")

    let recognise ts = match e ts with 
      | [] -> true 
      | _ -> false 
  end 

  module Parser = struct 
    let rec e = function 
      | FUN :: IDENT x :: ARROW :: toks -> 
          let (body, toks') = e toks in Fun(FParam x, body), toks'
      | LET :: IDENT x :: toks -> 
          begin match e toks with 
            | arg, IN :: toks' -> 
                let (body, toks'') = e toks' in 
                  Let(FParam x, arg, body), toks'' 
            | _ -> raise (ParseFail "LET") end 
      | LET :: REC :: IDENT x :: toks -> 
          begin match e toks with 
            | arg, IN :: toks' -> 
                let (body, toks'') = e toks' in 
                  LetRec(FParam x, arg, body), toks'' 
            | _ -> raise (ParseFail "LETREC") end 
      | toks -> t toks 

    and t' term = function 
      | EQUALS :: toks -> let term', toks' = f toks 
                        in t' (Equals(term, term')) toks'
      | toks -> term, toks 

    and t toks = let (term, toks') = f toks in 
                 t' term toks'

    and f' term = function
      | PLUS :: toks -> let term', toks' = g toks 
                        in f' (Plus(term, term')) toks'
      | toks -> term, toks

    and f toks = let (term, toks') = g toks in 
                 f' term toks'
    
    and g' term toks = match s toks with
      | (term', toks') -> g' (App(term, term')) toks'
      | exception ParseFail _ -> term, toks 
    
    and g toks = let (term, toks') = s toks in 
                 g' term toks'

    and s = function
      | IDENT x :: toks -> (Var x), toks 
      | NUM n :: toks -> (Num n), toks 
      | TRUE :: toks -> (Bool true), toks 
      | FALSE :: toks -> (Bool false), toks 
      | LPAREN :: toks -> 
        begin match e toks with 
            | term, RPAREN :: toks' -> term, toks'  
            | _ -> raise (ParseFail "S LPAREN") end 
      | _ -> raise (ParseFail "S")

    let parse ts = match e ts with 
      | expr, [] -> expr 
      | _ -> raise (ParseFail "failed")
  end 

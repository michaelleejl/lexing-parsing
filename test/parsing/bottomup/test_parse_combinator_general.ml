open Test_lexparse_parsing
open Lexparse.Mlot
open Lexparse.Parsing.Bottomup.Combinators
open Token
open General (Grammars.Natural)

let%expect_test "corpus" =
  Parse_cases.run parse;
  [%expect
    {|
    x                                x
    42                               42
    true                             true
    false                            false
    x + y                            Plus(x, y)
    1 + 2 + 3                        Plus(Plus(1, 2), 3)
    1 = 2 = 3                        Equals(Equals(1, 2), 3)
    1 + 2 = 3                        Equals(Plus(1, 2), 3)
    1 + 2 = 3 + 4                    Equals(Plus(1, 2), Plus(3, 4))
    f x                              App(f, x)
    f x y                            App(App(f, x), y)
    f x + g y                        Plus(App(f, x), App(g, y))
    1 2                              App(1, 2)
    (1 + 2) + 3                      Plus(Plus(1, 2), 3)
    1 + (2 + 3)                      Plus(1, Plus(2, 3))
    f (1 + 2)                        App(f, Plus(1, 2))
    ((x))                            x
    fun x -> x                       Fun(x, x)
    fun x -> x + 1                   Fun(x, Plus(x, 1))
    fun x -> fun y -> x + y          Fun(x, Fun(y, Plus(x, y)))
    (fun x -> x) 1                   App(Fun(x, x), 1)
    let x = 1 in x + 2               Let(x, 1, Plus(x, 2))
    let x = 1 in let y = 2 in x + y  Let(x, 1, Let(y, 2, Plus(x, y)))
    let rec f = fun x -> f x in f 1  LetRec(f, Fun(x, App(f, x)), App(f, 1))
    let x = fun y -> y in x 1        Let(x, Fun(y, y), App(x, 1))
    (let x = 1 in x) + 2             Plus(Let(x, 1, x), 2)
    fun x -> let y = x in y          Fun(x, Let(y, x, y))
    <empty>                          <rejected>
    1 +                              <rejected>
    + 1                              <rejected>
    x =                              <rejected>
    (1                               <rejected>
    1)                               <rejected>
    ()                               <rejected>
    in                               <rejected>
    fun -> x                         <rejected>
    fun x x                          <rejected>
    let x 1 in x                     <rejected>
    let x = 1 x                      <rejected>
    rec x = 1 in x                   <rejected>
    1 + fun x -> x                   <rejected>
    f let x = 1 in x                 <rejected>
    |}]

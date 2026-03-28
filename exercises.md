## Lexing 

### Lexer Combinators 
_In this section, we will build recogniser and lexer combinators._

1. Complete the Recogniser implementation in `lib/lexing/combinators.ml`
   _Hint:_  Write functions `emp`, `eps`, 
   `alt`, `seq` and `kleene` of type `r -> t`, and `one_of` of type 
   `C.t -> r -> t`, such that you can define `interpret` as follows
   ```ocaml 
   let rec interpret r =
    match r with
    | Empty -> emp
    | Epsilon -> eps
    | Char cs -> one_of cs
    | Alt (r1, r2) -> alt (interpret r1) (interpret r2)
    | Seq (r1, r2) -> seq (interpret r1) (interpret r2)
    | Kleene r -> kleene (interpret r)
    ```
    When defining `recognise`, note that you can convert 
    a string to a list of `char`s as follows
    ```ocaml
    Base.String.to_list 
    ```

2. Complete the Lexer implementation in `lib/lexing/combinators.ml`

   _Hint:_ The type `s` (type `t` in the previous question) now tracks 
   matched _characters_, in addition to the characters 
   yet to be matched. 
   Update your implementation from the earlier question accordingly.

   The type `t` now keeps track of matched _tokens_, rather than 
   matched _characters_. 
   The `action` type is the type of a function that tells us how 
   to turn a list of matched characters into a token. 
   Using your updated implementation, modify your old interpret function 
   to have type `r -> action -> t`. 

   The `( >>| )` function is like `alt`, but for type `t` rather than
   type `s`.

   The `lex` function has been written for you: it matches tokens, 
   one by one, until no more can be matched. 

3. We often know the lexer definition `l` before we know the string to be lexed `s`. How can we use this information to make our implementation more efficient?

### Lexer Generators 
1. How does compiling a lexer into an NFA differ from interpreting a lexer using OCaml functions?

2. In `lib/lexing/generators.ml`, write the `compile_regex` function, that compiles a regular expression into an NFA. 

3. In `lib/tdfa.ml`, write the determinise function, that converts a tagged NFA (in `tfna.ml`) to a tagged DFA

   _Hint._ If this question is proving troublesome, there is a `determinise` function in `nfa.ml` that might be worth consulting. 

4. In `lib/lexing/generators.ml`, write a `lex_step` function, that performs one step of lexing. 
   
   As described in lecture, this function should consume characters. When a failure state is reached, we should emit the token corresponding to the last accepting state, and then continue lexing. 

  ## Parsing 
  1. Consider the following language 
     $$
      \begin{array}{rcl}
      e & ::= & x \mid n \in \mathbb{N} \mid b \in \mathbb{B} \mid (e) \mid e_1 \, e_2 \mid e_1 + e_2 \mid e_1 = e_2 \\
      && \mid \texttt{fun} \, x \to e \mid \texttt{let} \, x = e_1 \, \texttt{in} \, e_2 \mid \texttt{let} \,\texttt{rec} \, x = e_1 \, \texttt{in} \, e_2 
      \end{array}$$
     For each of the following expressions, show that there are two different parses:

     a) $e_1 \, e_2 \, e_3$

     b) $\texttt{fun} \, x \to e_1 + e_2 $

2. Eliminate ambiguity in the language above. Do this by dividing the language above into simple expressions ($s$), terms ($t$), and expressions ($e$), such simple expressions are terms, and terms are expressions, as such:
    $$\begin{array}{rcl}
      e & := & \ldots \mid t \\
      t & := & \ldots \mid s \\
      s & := & \ldots
    \end{array}$$
    Thus, terms $t$ are expressions $e$. Rules are ordered by priority. Rules to the left have higher priority than rules to the right. Thus, when trying to create an expression $e$, we will only try to create a term $t$ if none of the other $e$ rules match.

    With reference to question 1, show how your new rules eliminate ambiguity. 

3. Refactor your grammar in question 2 to eliminate left-recursion
open Intfs
open Intfs.Language

module Recogniser : sig
  type recogniser = char list -> char list outcome

  val char_recogniser : (char -> bool) -> recogniser
  val alphabetic : recogniser
  val numeric : recogniser
  val alphanumeric : recogniser
  val whitespace : recogniser
  val empty : recogniser
  val epsilon : recogniser
  val ( >& ) : recogniser -> recogniser -> recogniser
  val ( >| ) : recogniser -> recogniser -> recogniser
  val ( ~* ) : recogniser -> recogniser
  val ( ~+ ) : recogniser -> recogniser
  val ( ~? ) : recogniser -> recogniser
  val from_str : string -> recogniser
  val from_str_list : string list -> recogniser
  val recognise : recogniser -> string -> bool
end

module Lexer : (Lang : L) -> sig
  module Matcher : sig
    type matcher_state = { matched : char list; rest : char list }
    type matcher = char list -> matcher_state outcome

    val char_matcher : (char -> bool) -> matcher
    val alphabetic : matcher
    val numeric : matcher
    val alphanumeric : matcher
    val whitespace : matcher
    val empty : matcher
    val epsilon : matcher
    val ( >& ) : matcher -> matcher -> matcher
    val ( >| ) : matcher -> matcher -> matcher
    val ( ~* ) : matcher -> matcher
    val ( ~+ ) : matcher -> matcher
    val ( ~? ) : matcher -> matcher
    val from_str : string -> matcher
  end

  exception LexFailure

  type token = Lang.token
  type action = char list -> token option
  type matcher = Matcher.matcher
  type lex_state = { lexed : token list; rest : char list }
  type lexer = lex_state -> lex_state outcome

  val promote : matcher -> action -> lexer
  val empty : lexer
  val epsilon : lexer
  val ( >>& ) : lexer -> lexer -> lexer
  val ( >>| ) : lexer -> lexer -> lexer
  val ( ~~* ) : lexer -> lexer
  val ( ~~+ ) : lexer -> lexer
  val ( ~~? ) : lexer -> lexer
  val lex : lexer -> string -> token list
end

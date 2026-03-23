open Intfs
open Intfs.Language

module Recogniser : sig
  type t
  type s

  val empty : t
  val epsilon : t
  val chr : char -> t
  val str : string -> t
  val ( >| ) : t -> t -> t
  val ( >& ) : t -> t -> t
  val ( ~* ) : t -> t
  val ( ~+ ) : t -> t
  val ( ~? ) : t -> t
  val r : string -> t
  val interpret : t -> s
  val recognise : s -> string -> bool
end

module Lexer : (Lang : L) -> sig
  type token = Lang.token
  type action = char list -> token option

  exception LexFailure

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

  type matcher = Matcher.matcher
  type lex_state = { lexed : token list; rest : char list }
  type lexer = lex_state -> lex_state outcome

  val tag : matcher -> action -> lexer
  val empty : lexer
  val epsilon : lexer
  val ( >>& ) : lexer -> lexer -> lexer
  val ( >>| ) : lexer -> lexer -> lexer
  val ( ~~* ) : lexer -> lexer
  val ( ~~+ ) : lexer -> lexer
  val ( ~~? ) : lexer -> lexer
  val lex : lexer -> string -> token list
end

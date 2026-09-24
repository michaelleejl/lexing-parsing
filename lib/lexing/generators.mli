open Lang
open Regex

module Recogniser : sig
  type r = Regex.t
  type t (* the type of recognisers *)

  val compile : r -> t
  val recognise : t -> string -> bool
end

module Lexer
    (Spec : LEXICAL_SPEC with type input = char and type spec = Charset.t regex) : sig
  type token = Spec.token

  exception LexFailure of string

  val lex : string -> token list
end

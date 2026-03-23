open Intfs
open Intfs.Language

module Recogniser : sig
  type r = Regex.t
  type t (* the type of recognisers *)

  val interpret : r -> t
  val recognise : t -> string -> bool
end

module Lexer : (Lang : L) -> sig
  type token = Lang.token
  type action = char list -> token option
  type r = Regex.t
  type t (* the type of lexers *)

  exception LexFailure

  val interpret : r -> action -> t
  val ( >>| ) : t -> t -> t
  val lex : t -> string -> token list
end

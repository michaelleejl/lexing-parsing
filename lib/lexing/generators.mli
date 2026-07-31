open Intfs
open Regex 

module Recogniser : sig
  type r = Regex.t
  type t (* the type of recognisers *)

  val compile : r -> t
  val recognise : t -> string -> bool
end

module Lexer
    (Lang : Language.S) (Vocab: Vocabulary.S with type output = Lang.token option and type input = char and type spec = C.t rgx) : sig
  
  type token = Lang.token

  exception LexFailure of string

  val lex : string -> token list
end

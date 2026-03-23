open Intfs
open Intfs.Language
open Intfs.Tags

module Recogniser : sig
  type r = Regex.t
  type t (* the type of recognisers *)

  val compile : r -> t
  val recognise : t -> string -> bool
end

module Lexer (Lang : L) (Tag : T with type token = Lang.token) : sig
  type tag = Tag.t
  type token = Lang.token
  type r = Regex.t
  type s (* the type of nondeterministic lexers *)
  type t (* the type of deterministic lexers *)

  exception LexFailure of string

  val compile : r -> tag -> s
  val ( >>| ) : s -> s -> s
  val determinise : s -> t
  val lex : t -> string -> token list
end

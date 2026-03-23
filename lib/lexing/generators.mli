open Intfs
open Intfs.Language
open Intfs.Tags

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
  val compile : t -> s
  val recognise : s -> string -> bool
end

module Lexer (Lang : L) (Tag : T with type token = Lang.token) : sig
  type m
  type t
  type s
  type tag_t = Tag.t
  type token = Lang.token

  exception LexFailure of string

  module Matcher : sig
    val empty : m
    val epsilon : m
    val chr : char -> m
    val str : string -> m
    val ( >| ) : m -> m -> m
    val ( >& ) : m -> m -> m
    val ( ~* ) : m -> m
    val ( ~+ ) : m -> m
    val ( ~? ) : m -> m
    val r : string -> m
  end

  val tag : m -> tag_t -> s
  val ( >>| ) : s -> s -> s
  val compile : s -> t
  val lex : t -> string -> token list
end

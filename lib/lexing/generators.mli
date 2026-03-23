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
  val parse : string -> t
  val generate : t -> s
  val recognise : s -> string -> bool
end

module Lexer (Lang : L) (Tag : T with type token = Lang.token) : sig
  type m
  type t
  type s
  type tag = Tag.t
  type token = Lang.token

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
    val parse : string -> m
  end

  val create : m -> tag -> s
  val ( >>| ) : s -> s -> s
  val determinise : s -> t
  val lex : t -> string -> token list
end

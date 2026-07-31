open Intfs

module Recogniser : sig
  type r = Regex.t
  type t (* the type of recognisers *)

  val compile : r -> t
  val recognise : t -> string -> bool
end

module Lexer
    (Lang : Language.S) : sig
  
  module LexTag: sig 

    type t = int 

    val new_tag: unit -> t

    type args = char list 
    type output = Lang.token option 

    type tag_table = (int, args -> output) Hashtbl.t 
    val compare: t -> t -> t

    val register_tag: t -> (args -> output) -> unit  

    val tag_to_action: t -> args -> output 
  end 

  type tag = LexTag.t
  type action = LexTag.args -> LexTag.output
  type token = Lang.token
  type r = Regex.t
  type s (* the type of nondeterministic lexers *)
  type t (* the type of deterministic lexers *)

  exception LexFailure of string

  val compile : r -> action -> s
  val ( >>| ) : s -> s -> s
  val determinise : s -> t
  val lex : t -> string -> token list
end

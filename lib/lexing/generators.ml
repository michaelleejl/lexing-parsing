module Recogniser = struct
  open Regex

  type t = Regex.t
  type s = Dfa.t

  let empty = empty
  let epsilon = epsilon
  let chr = chr
  let str = str
  let ( >| ) = alt
  let ( >& ) = seq
  let ( ~* ) = kleene
  let ( ~+ ) = plus
  let ( ~? ) = opt
  let parse = parse
  let generate r = Regex.compile r |> Dfa.determinise
  let recognise = Dfa.accept
end

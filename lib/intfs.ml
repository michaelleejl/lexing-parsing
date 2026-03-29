type 'a outcome = Success of 'a | Failure

module type Ast = sig
  type fparam
  type node
end

module type Token = sig
  type t
end

module Language = struct
  module type S = sig
    type token
    type ast
    type fparam
  end

  module Make (S : Token) (A : Ast) = struct
    type token = S.t
    type ast = A.node
    type fparam = A.fparam
  end
end

module Tags = struct
  module type S = sig
    type t
    type token

    val compare : t -> t -> int
    val tag_to_action : t -> char list -> token option
  end
end
 
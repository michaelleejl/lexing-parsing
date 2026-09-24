open Ppx_compare_lib.Builtin

(* Small hand-built grammars for exercising the cycle check in
   [Parsing.Analysis.Grammar_analysis.Cycles].

   A grammar is *cyclic* when some nonterminal derives itself and nothing
   else: [A =>+ A]. That is strictly narrower than left recursion, and it is
   the case that makes a general LR parser loop forever, since the reduction
   consumes no input.

   Every grammar below shares one alphabet so a single functor can build them;
   the productions are what varies. Builders are never run here — only the
   grammar analysis is — so they all return [Nothing]. *)

module Nonterminal = struct
  type t = S | A | B | C [@@deriving compare, to_string]
end

module Terminal = struct
  type t = NUM | PLUS | EOF [@@deriving compare, to_string]
end

type rhs_item = [ `T of Terminal.t | `N of Nonterminal.t ]

module Make (P : sig
  val rules : (Nonterminal.t * rhs_item list) list
end) =
struct
  exception Fail

  type token = Lexparse.Mlot.Token.t [@@deriving compare]
  type ast = Lexparse.Mlot.Ast.node [@@deriving compare]

  module Terminal = Terminal

  type terminal = Terminal.t [@@deriving compare, to_string]

  module Data = struct
    type t = Nothing [@@deriving compare]
  end

  type data = Data.t [@@deriving compare]
  type builder = data list -> data
  type reader = token -> data

  let finish _ = raise Fail
  let build b ds = b ds
  let read r tok = r tok

  let token_to_terminal (t : token) =
    match t with
    | NUM _ -> Terminal.NUM
    | PLUS -> Terminal.PLUS
    | _ -> Terminal.EOF

  let eof = Lexparse.Mlot.Token.EOF

  module Nonterminal = Nonterminal

  type nonterminal = Nonterminal.t [@@deriving compare, to_string]

  module Sym = struct
    type t =
      | T of terminal [@stringable.nested ""]
      | N of nonterminal [@stringable.nested ""]
    [@@deriving compare, to_string]
  end

  type sym = Sym.t =
    | T of terminal [@stringable.nested ""]
    | N of nonterminal [@stringable.nested ""]
  [@@deriving compare, to_string]

  type production = { lhs : nonterminal; rhs : sym list; builder : builder }
  type consumption = { lhs : terminal; reader : reader }

  let nothing _ = Data.Nothing
  let item : rhs_item -> sym = function `T t -> T t | `N n -> N n

  let productions =
    List.map
      (fun (lhs, rhs) -> { lhs; rhs = List.map item rhs; builder = nothing })
      P.rules

  let start = Nonterminal.S

  let consumptions =
    [
      { lhs = Terminal.NUM; reader = nothing };
      { lhs = Terminal.PLUS; reader = nothing };
      { lhs = Terminal.EOF; reader = nothing };
    ]
end

(* ---------- cyclic ---------- *)

(* S ::= S | NUM              S =>+ S directly *)
module Self_cycle = Make (struct
  let rules =
    [
      (Nonterminal.S, [ `N Nonterminal.S ]); (Nonterminal.S, [ `T Terminal.NUM ]);
    ]
end)

(* S ::= A | NUM ,  A ::= S   S => A => S *)
module Unit_cycle = Make (struct
  let rules =
    [
      (Nonterminal.S, [ `N Nonterminal.A ]);
      (Nonterminal.S, [ `T Terminal.NUM ]);
      (Nonterminal.A, [ `N Nonterminal.S ]);
    ]
end)

(* S ::= B A C | NUM ,  A ::= S ,  B ::= eps ,  C ::= eps
   S => B A C => A => S, with B and C vanishing on the way.
   No cycle exists in the unit productions alone, so a check that only looks
   at rules of the form [X ::= Y] misses this one. *)
module Padded_cycle = Make (struct
  let rules =
    [
      (Nonterminal.S, [ `N Nonterminal.B; `N Nonterminal.A; `N Nonterminal.C ]);
      (Nonterminal.S, [ `T Terminal.NUM ]);
      (Nonterminal.A, [ `N Nonterminal.S ]);
      (Nonterminal.B, []);
      (Nonterminal.C, []);
    ]
end)

(* ---------- acyclic ---------- *)

(* S ::= S PLUS A | A ,  A ::= NUM
   Left-recursive, and deliberately so — this is how left association is
   expressed. [PLUS A] cannot vanish, so S never derives itself alone. *)
module Left_recursive = Make (struct
  let rules =
    [
      (Nonterminal.S, [ `N Nonterminal.S; `T Terminal.PLUS; `N Nonterminal.A ]);
      (Nonterminal.S, [ `N Nonterminal.A ]);
      (Nonterminal.A, [ `T Terminal.NUM ]);
    ]
end)

(* S ::= A B ,  B ::= eps | S ,  A ::= NUM
   The shape of [G' ::= S G'] in Grammars.Ll1: a non-nullable head followed by
   a nullable tail that refers back. Reaching S again requires A to vanish,
   and it cannot, so this is acyclic. *)
module Nullable_tail = Make (struct
  let rules =
    [
      (Nonterminal.S, [ `N Nonterminal.A; `N Nonterminal.B ]);
      (Nonterminal.B, []);
      (Nonterminal.B, [ `N Nonterminal.S ]);
      (Nonterminal.A, [ `T Terminal.NUM ]);
    ]
end)

(* S ::= A B ,  A ::= eps ,  B ::= NUM
   Nullable nonterminals, no cycle — a control for the two above. *)
module Nullable_prefix = Make (struct
  let rules =
    [
      (Nonterminal.S, [ `N Nonterminal.A; `N Nonterminal.B ]);
      (Nonterminal.A, []);
      (Nonterminal.B, [ `T Terminal.NUM ]);
    ]
end)

open Lang
open Items
open Fixpoint
open Effect
open Effect.Deep
open Ppx_compare_lib.Builtin

exception Parse_error of string

module Generalised (Grammar : GRAMMAR) = struct
  module Augmented = Bottomup_augment (Grammar)
  module Bnf = Augmented.Bnf
  open Augmented
  open Bnf
  open Views (Bnf)

  type token = Augmented.token [@@deriving compare]
  type ast = Augmented.ast [@@deriving compare]

  module Item = Lr0.Make (Bnf)
  open Item
  module Item_set = Set.Make (Item)

  let closure items =
    Fixpoint.fix ~eq:Item_set.equal
      (fun its ->
        Item_set.fold
          (fun item acc -> Item_set.union (Item_set.of_list @@ eps item) acc)
          its its)
      items

  exception Accepted of ast

  type parse_state = { items : Item_set.t; datum : data; tokens : token list }
  type act = Shift | Reduce of production | No_action

  type reduction = {
    production : production;
    dot : int;
    args : data list;
    tokens : token list;
  }

  type _ Effect.t += Reduce_result : reduction -> unit Effect.t

  let goto items sym =
    Item_set.fold
      (fun item acc ->
        match advance item sym with
        | None -> acc
        | Some item' -> Item_set.add item' acc)
      items Item_set.empty

  let shift { tokens; items } =
    match tokens with
    | token :: tokens ->
        let terminal = token_to_terminal token in
        let datum = Augmented.read (reader_of_terminal terminal) token in
        let new_items = goto items (T terminal) in
        { tokens; datum; items = new_items }
    | _ -> raise (Parse_error "token mismatch")

  let accept production args =
    raise (Accepted (finish (build (builder_of_production production) args)))

  let action item tokens =
    match next item with
    | None -> Reduce (production_of item)
    | Some (T t) ->
        begin match tokens with
        | [] -> No_action
        | tok :: _ ->
            let t' = token_to_terminal tok in
            if t <> t' then No_action else Shift
        end
    | _ -> No_action

  let actions state tokens =
    Item_set.fold
      (fun item (reduces, shifts) ->
        match action item tokens with
        | No_action -> (reduces, shifts)
        | Shift -> (reduces, true)
        | Reduce p -> (p :: reduces, shifts))
      state ([], false)

  let retry k = continue k

  let rec parser ({ items; datum; tokens } as parse_state) =
    let state = closure items in
    let rec unwind ({ production; dot; args; tokens } as reduction) alts =
      let fallback = retry alts in
      if dot > 0 then (
        collect reduction;
        fallback ())
      else if production.lhs <> start then
        complete production args tokens fallback
      else
        match tokens with
        | [ t ] when t = eof -> accept production args
        | _ -> fallback ()
    and collect reduction =
      perform
        (Reduce_result
           {
             reduction with
             dot = reduction.dot - 1;
             args = datum :: reduction.args;
           })
    and complete production args tokens fallback =
      next
        {
          items = goto state (N production.lhs);
          datum = build (builder_of_production production) args;
          tokens;
        }
        fallback
    and next state fallback =
      try
        match parser state with
        | v -> v
        | effect Reduce_result reduction, k -> unwind reduction k
      with Parse_error _ -> fallback ()
    in
    if Item_set.is_empty state then raise (Parse_error "empty state")
    else
      let rec try_act rs s =
        match rs with
        | [] ->
            if s then
              match parser (shift { parse_state with items = state }) with
              | v -> v
              | effect Reduce_result reduction, k -> unwind reduction k
            else raise (Parse_error "no more actions")
        | production :: rs ->
            let fallback () = try_act rs s in
            if List.is_empty production.rhs then
              complete production [] tokens fallback
            else (
              collect
                {
                  production;
                  dot = List.length production.rhs;
                  args = [];
                  tokens;
                };
              fallback ())
      in
      let reduces, shifts = actions state tokens in
      try_act reduces shifts

  let parse ts =
    try
      match
        parser
          {
            items = Item_set.singleton items.start;
            datum = Data.start;
            tokens = ts @ [ eof ];
          }
      with
      | v -> v
      | effect Reduce_result _, _ -> raise (Parse_error "stack underflow")
    with Accepted ast -> ast
end

module Slr1 (Grammar : GRAMMAR) = struct
  module Augmented = Bottomup_augment (Grammar)
  module Bnf = Augmented.Bnf
  open Augmented
  open Bnf
  open Views (Bnf)

  type token = Augmented.token [@@deriving compare]
  type ast = Augmented.ast [@@deriving compare]

  module Item = Lr0.Make (Bnf)
  open Item
  module Item_set = Set.Make (Item)

  let closure items =
    Fixpoint.fix ~eq:Item_set.equal
      (fun its ->
        Item_set.fold
          (fun item acc -> Item_set.union (Item_set.of_list @@ eps item) acc)
          its its)
      items

  type parse_state = { items : Item_set.t; datum : data; tokens : token list }
  type act = Shift | Reduce of production | No_action

  type reduction = {
    production : production;
    dot : int;
    args : data list;
    tokens : token list;
  }

  type result = Partial of reduction | Accepted of ast

  let goto items sym =
    Item_set.fold
      (fun item acc ->
        match advance item sym with
        | None -> acc
        | Some item' -> Item_set.add item' acc)
      items Item_set.empty

  let shift { tokens; items } =
    match tokens with
    | token :: tokens ->
        let terminal = token_to_terminal token in
        let datum = Augmented.read (reader_of_terminal terminal) token in
        let new_items = goto items (T terminal) in
        { tokens; datum; items = new_items }
    | _ -> raise (Parse_error "token mismatch")

  let accept production args =
    Accepted (finish (build (builder_of_production production) args))

  let action item tokens =
    match tokens with
    | [] -> raise (Parse_error "unexpected end of input")
    | tok :: _ -> (
        let t' = token_to_terminal tok in
        match next item with
        | None ->
            if may_reduce_on item t' then Reduce (production_of item)
            else No_action
        | Some (T t) -> if t <> t' then No_action else Shift
        | _ -> No_action)

  let actions state tokens =
    Item_set.fold
      (fun item (reduces, shifts) ->
        match action item tokens with
        | No_action -> (reduces, shifts)
        | Shift -> (reduces, true)
        | Reduce p -> (p :: reduces, shifts))
      state ([], false)

  let rec parser ({ items; datum; tokens } as parse_state) =
    let state = closure items in
    let rec unwind = function
      | Accepted e -> Accepted e
      | Partial ({ production; dot; args; tokens } as reduction) -> (
          if dot > 0 then collect reduction
          else if production.lhs <> start then complete production args tokens
          else
            match tokens with
            | [ t ] when t = eof -> accept production args
            | _ -> raise (Parse_error "trailing input"))
    and collect reduction =
      Partial
        {
          reduction with
          dot = reduction.dot - 1;
          args = datum :: reduction.args;
        }
    and complete production args tokens =
      next
        {
          items = goto state (N production.lhs);
          datum = build (builder_of_production production) args;
          tokens;
        }
    and next state = unwind (parser state) in
    if Item_set.is_empty state then raise (Parse_error "empty state")
    else
      let act rs s =
        match (rs, s) with
        | [], false -> raise (Parse_error "no actions")
        | [], true -> unwind (parser (shift { parse_state with items = state }))
        | [ production ], false ->
            if List.is_empty production.rhs then complete production [] tokens
            else
              collect
                {
                  production;
                  dot = List.length production.rhs;
                  args = [];
                  tokens;
                }
        | _ :: _, true -> raise (Parse_error "shift reduce conflict")
        | _ :: _, false -> raise (Parse_error "reduce reduce conflict")
      in
      let reduces, shifts = actions state tokens in
      act reduces shifts

  let parse ts =
    match
      parser
        {
          items = Item_set.singleton items.start;
          datum = Data.start;
          tokens = ts @ [ eof ];
        }
    with
    | Accepted ast -> ast
    | _ -> raise (Parse_error "no valid parse")
end

module Lr1 (Grammar : GRAMMAR) = struct
  module Augmented = Bottomup_augment (Grammar)
  module Bnf = Augmented.Bnf
  open Augmented
  open Bnf
  open Views (Bnf)

  type token = Augmented.token [@@deriving compare]
  type ast = Augmented.ast [@@deriving compare]

  module Item = Items.Lr1.Make (Bnf)
  open Item
  module Item_set = Set.Make (Item)

  let closure items =
    Fixpoint.fix ~eq:Item_set.equal
      (fun its ->
        Item_set.fold
          (fun item acc -> Item_set.union (Item_set.of_list @@ eps item) acc)
          its its)
      items

  type parse_state = { items : Item_set.t; datum : data; tokens : token list }
  type act = Shift | Reduce of production | No_action

  type reduction = {
    production : production;
    dot : int;
    args : data list;
    tokens : token list;
  }

  type result = Partial of reduction | Accepted of ast

  let goto items sym =
    Item_set.fold
      (fun item acc ->
        match advance item sym with
        | None -> acc
        | Some item' -> Item_set.add item' acc)
      items Item_set.empty

  let shift { tokens; items } =
    match tokens with
    | token :: tokens ->
        let terminal = token_to_terminal token in
        let datum = Augmented.read (reader_of_terminal terminal) token in
        let new_items = goto items (T terminal) in
        { tokens; datum; items = new_items }
    | _ -> raise (Parse_error "token mismatch")

  let accept production args =
    Accepted (finish (build (builder_of_production production) args))

  let action item tokens =
    match tokens with
    | [] -> raise (Parse_error "unexpected end of input")
    | tok :: _ -> (
        let t' = token_to_terminal tok in
        match next item with
        | None ->
            if may_reduce_on item t' then Reduce (production_of item)
            else No_action
        | Some (T t) -> if t <> t' then No_action else Shift
        | _ -> No_action)

  let actions state tokens =
    Item_set.fold
      (fun item (reduces, shifts) ->
        match action item tokens with
        | No_action -> (reduces, shifts)
        | Shift -> (reduces, true)
        | Reduce p -> (p :: reduces, shifts))
      state ([], false)

  let rec parser ({ items; datum; tokens } as parse_state) =
    let state = closure items in
    let rec unwind = function
      | Accepted e -> Accepted e
      | Partial ({ production; dot; args; tokens } as reduction) -> (
          if dot > 0 then collect reduction
          else if production.lhs <> start then complete production args tokens
          else
            match tokens with
            | [ t ] when t = eof -> accept production args
            | _ -> raise (Parse_error "trailing input"))
    and collect reduction =
      Partial
        {
          reduction with
          dot = reduction.dot - 1;
          args = datum :: reduction.args;
        }
    and complete production args tokens =
      next
        {
          items = goto state (N production.lhs);
          datum = build (builder_of_production production) args;
          tokens;
        }
    and next state = unwind (parser state) in
    if Item_set.is_empty state then raise (Parse_error "empty state")
    else
      let act rs s =
        match (rs, s) with
        | [], false -> raise (Parse_error "no actions")
        | [], true -> unwind (parser (shift { parse_state with items = state }))
        | [ production ], false ->
            if List.is_empty production.rhs then complete production [] tokens
            else
              collect
                {
                  production;
                  dot = List.length production.rhs;
                  args = [];
                  tokens;
                }
        | _ :: _, true -> raise (Parse_error "shift reduce conflict")
        | _ :: _, false -> raise (Parse_error "reduce reduce conflict")
      in
      let reduces, shifts = actions state tokens in
      act reduces shifts

  let parse ts =
    match
      parser
        {
          items = Item_set.singleton items.start;
          datum = Data.start;
          tokens = ts @ [ eof ];
        }
    with
    | Accepted ast -> ast
    | _ -> raise (Parse_error "no valid parse")
end

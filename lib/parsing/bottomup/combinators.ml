open Lang
open Items
open Fixpoint
open Effect
open Effect.Deep
open Ppx_compare_lib.Builtin

exception ParseFail of string

module General (Grammar : GRAMMAR) = struct
  module Elaborated = Elaborate (Grammar)
  module Bnf = Elaborated.Bnf
  open Elaborated
  open Bnf
  open Views (Bnf)

  type token = Elaborated.token [@@deriving compare]
  type ast = Elaborated.ast [@@deriving compare]

  module Item = LR0.Make (Bnf)
  open Item
  module ItemSet = Set.Make (Item)

  exception ParseError of string

  let closure items =
    Fixpoint.fix ~eq:ItemSet.equal
      (fun its ->
        ItemSet.fold
          (fun item acc -> ItemSet.union (ItemSet.of_list @@ eps item) acc)
          its its)
      items

  exception Accepted of ast

  type parse_state = { items : ItemSet.t; datum : data; tokens : token list }
  type act = Shift | Reduce of production | Nothing

  type result = {
    production : production;
    dot : int;
    args : data list;
    tokens : token list;
  }

  type _ Effect.t += ReduceResult : result -> unit Effect.t

  let shift_sym_on_items items sym =
    ItemSet.fold
      (fun item acc ->
        match advance item sym with
        | None -> acc
        | Some item' -> ItemSet.add item' acc)
      items ItemSet.empty

  let shift { tokens; items } =
    match tokens with
    | token :: tokens ->
        let terminal = token_to_terminal token in
        let datum = Elaborated.read (reader_of_terminal terminal) token in
        let new_items = shift_sym_on_items items (T terminal) in
        { tokens; datum; items = new_items }
    | _ -> raise (ParseError "token mismatch")

  let accept production args =
    raise (Accepted (finish (build (builder_of_production production) args)))

  let action item tokens =
    match next item with
    | None -> Reduce (production_of item)
    | Some (T t) ->
        begin match tokens with
        | [] -> Nothing
        | tok :: _ ->
            let t' = token_to_terminal tok in
            if t <> t' then Nothing else Shift
        end
    | _ -> Nothing

  let actions state tokens =
    ItemSet.fold
      (fun item (reduces, shifts) ->
        match action item tokens with
        | Nothing -> (reduces, shifts)
        | Shift -> (reduces, true)
        | Reduce p -> (p :: reduces, shifts))
      state ([], false)

  let retry k = continue k

  let rec parser ({ items; datum; tokens } as parse_state) =
    let state = closure items in
    let rec handle_reduce ({ production; dot; args; tokens } as result) alts =
      let fallback = retry alts in
      if dot > 0 then (
        collect result;
        fallback ())
      else if production.lhs <> start then goto production args tokens fallback
      else match tokens with [] -> accept production args | _ -> fallback ()
    and collect result =
      perform
        (ReduceResult
           { result with dot = result.dot - 1; args = datum :: result.args })
    and goto production args tokens fallback =
      next
        {
          items = shift_sym_on_items state (N production.lhs);
          datum = build (builder_of_production production) args;
          tokens;
        }
        fallback
    and next state fallback =
      try
        match parser state with
        | v -> v
        | effect ReduceResult result, k -> handle_reduce result k
      with ParseError _ -> fallback ()
    in
    if ItemSet.is_empty state then raise (ParseError "empty state")
    else
      let rec try_act rs s =
        match rs with
        | [] ->
            if s then
              match parser (shift { parse_state with items = state }) with
              | v -> v
              | effect ReduceResult result, k -> handle_reduce result k
            else raise (ParseError "no more actions")
        | production :: rs ->
            let fallback () = try_act rs s in
            if List.is_empty production.rhs then
              goto production [] tokens fallback
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
            items = ItemSet.singleton items.start;
            datum = Data.start;
            tokens = ts @ [ eof ];
          }
      with
      | v -> v
      | effect ReduceResult _, _ -> raise (ParseError "stack underflow")
    with Accepted ast -> ast
end

module SLR1 (Grammar : GRAMMAR) = struct
  module Elaborated = Elaborate (Grammar)
  module Bnf = Elaborated.Bnf
  open Elaborated
  open Bnf
  open Views (Bnf)

  type token = Elaborated.token [@@deriving compare]
  type ast = Elaborated.ast [@@deriving compare]

  module Item = LR0.Make (Bnf)
  open Item
  module ItemSet = Set.Make (Item)

  exception ParseError of string

  let closure items =
    Fixpoint.fix ~eq:ItemSet.equal
      (fun its ->
        ItemSet.fold
          (fun item acc -> ItemSet.union (ItemSet.of_list @@ eps item) acc)
          its its)
      items

  type parse_state = { items : ItemSet.t; datum : data; tokens : token list }
  type act = Shift | Reduce of production | Nothing

  type result = {
    production : production;
    dot : int;
    args : data list;
    tokens : token list;
  }

  type outcome = Pending of result | Accepted of ast

  let shift_sym_on_items items sym =
    ItemSet.fold
      (fun item acc ->
        match advance item sym with
        | None -> acc
        | Some item' -> ItemSet.add item' acc)
      items ItemSet.empty

  let shift { tokens; items } =
    match tokens with
    | token :: tokens ->
        let terminal = token_to_terminal token in
        let datum = Elaborated.read (reader_of_terminal terminal) token in
        let new_items = shift_sym_on_items items (T terminal) in
        { tokens; datum; items = new_items }
    | _ -> raise (ParseError "token mismatch")

  let accept production args =
    Accepted (finish (build (builder_of_production production) args))

  let action item tokens =
    match tokens with
    | [] -> if is_accept item then Reduce (production_of item) else Nothing
    | tok :: _ -> (
        let t' = token_to_terminal tok in
        match next item with
        | None ->
            if is_valid_for item t' then Reduce (production_of item)
            else Nothing
        | Some (T t) -> if t <> t' then Nothing else Shift
        | _ -> Nothing)

  let actions state tokens =
    ItemSet.fold
      (fun item (reduces, shifts) ->
        match action item tokens with
        | Nothing -> (reduces, shifts)
        | Shift -> (reduces, true)
        | Reduce p -> (p :: reduces, shifts))
      state ([], false)

  let rec parser ({ items; datum; tokens } as parse_state) =
    let state = closure items in
    let rec handle_reduce = function
      | Accepted e -> Accepted e
      | Pending ({ production; dot; args; tokens } as result) -> (
          if dot > 0 then collect result
          else if production.lhs <> start then goto production args tokens
          else
            match tokens with
            | [] -> accept production args
            | _ -> raise (ParseError "trailing input"))
    and collect result =
      Pending { result with dot = result.dot - 1; args = datum :: result.args }
    and goto production args tokens =
      next
        {
          items = shift_sym_on_items state (N production.lhs);
          datum = build (builder_of_production production) args;
          tokens;
        }
    and next state = handle_reduce (parser state) in
    if ItemSet.is_empty state then raise (ParseError "empty state")
    else
      let act rs s =
        match (rs, s) with
        | [], false -> raise (ParseError "no actions")
        | [], true ->
            handle_reduce (parser (shift { parse_state with items = state }))
        | [ production ], false ->
            if List.is_empty production.rhs then goto production [] tokens
            else
              collect
                {
                  production;
                  dot = List.length production.rhs;
                  args = [];
                  tokens;
                }
        | _ :: _, true -> raise (ParseError "shift reduce conflict")
        | _ :: _, false -> raise (ParseError "reduce reduce conflict")
      in
      let reduces, shifts = actions state tokens in
      act reduces shifts

  let parse ts =
    match
      parser
        {
          items = ItemSet.singleton items.start;
          datum = Data.start;
          tokens = ts @ [ eof ];
        }
    with
    | Accepted ast -> ast
    | _ -> raise (ParseFail "no valid parse")
end


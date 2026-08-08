open Intfs
open Ppx_compare_lib.Builtin

exception ParseFail of string

module General (Gram : Grammar.S) = struct
  open Gram

  type token = Gram.token
  type ast = Gram.ast

  module ReductionRegistry =
    Registry.Make
      (struct
        type t = Gram.reduce
      end)

  module ParseStack = struct
    type frame = {
      remaining : t list;
      collected : data list;
      action : ReductionRegistry.Tag.t;
    }
    [@@deriving compare]

    type t = frame list [@@deriving compare]

    let create_frame remaining action = { remaining; collected = []; action }
    let is_filled { remaining } = List.length remaining = 0

    let fill_frame { remaining; collected; action } data =
      match remaining with
      | r :: rs -> { remaining = rs; collected = data :: collected; action }
      | [] -> raise (ParseFail "already filled")

    let rec normalise = function
      | [] -> []
      | [ f ] -> [ f ]
      | f1 :: f2 :: fs as frames ->
          if is_filled f1 then
            let r = ReductionRegistry.get f1.action in
            let d = r (List.rev f1.collected) in
            normalise (fill_frame f2 d :: fs)
          else frames

    let empty = []

    let fill s d =
      match s with
      | [] -> raise (ParseFail "cannot fill an empty stack")
      | f :: fs -> normalise (fill_frame f d :: fs)

    let push s f = normalise (f :: s)

    let unwrap = function
      | [] -> raise (ParseFail "cannot unwrap empty stack")
      | [ f ] ->
          if is_filled f then
            let r = ReductionRegistry.get f.action in
            r (List.rev f.collected)
          else raise (ParseFail "cannot unwrap partially filled frame")
      | _ -> raise (ParseFail "cannot unwrap non-singleton stack")
  end

  module ActionRegistry =
    Registry.Make
      (struct
        type t = token -> ParseStack.t -> ParseStack.t
      end)

  let prod_action syms reduce =
   fun _ s ->
    let f = ParseStack.create_frame syms reduce in
    ParseStack.push s f

  let cons_action shift = fun token s -> ParseStack.fill s (shift token)
  let register_cons shift = ActionRegistry.register (cons_action shift)

  let register_prod syms reduce =
    let rtag = ReductionRegistry.register reduce in
    ActionRegistry.register (prod_action syms rtag)

  module TaggedPda = Automata.Tpda.Make (Terminal) (Gram) (ActionRegistry.Tag)

  module ParseHypothesis = struct
    type t = TaggedPda.config * ParseStack.t [@@deriving compare]
  end

  module ParseHypotheses = Set.Make (ParseHypothesis)

  type parsing_state = { tokens : token list; hypotheses : ParseHypotheses.t }

  let rec evolve_stack token tags stack =
    match tags with
    | [] -> stack
    | t :: ts ->
        let action = ActionRegistry.get t in
        evolve_stack token ts (action token stack)

  let collect_traces token stack (new_cfg, trace) hypotheses =
    let new_stack = evolve_stack token trace stack in
    let hypothesis = (new_cfg, new_stack) in
    ParseHypotheses.add hypothesis hypotheses

  let advance_one machine token ((config, stack) : ParseHypothesis.t) =
    let traces =
      TaggedPda.consume machine
        (TaggedPda.TraceSet.singleton (config, []))
        (Gram.token_to_terminal token)
    in
    TaggedPda.TraceSet.fold
      (collect_traces token stack)
      traces ParseHypotheses.empty

  let advance machine token hypotheses =
    ParseHypotheses.fold
      (fun hyp ->
        fun hyps ->
         let hyps' = advance_one machine token hyp in
         ParseHypotheses.union hyps hyps')
      hypotheses ParseHypotheses.empty

  let parse_step machine tok { tokens; hypotheses } =
    let new_hypotheses = advance machine tok hypotheses in
    { tokens; hypotheses = new_hypotheses }

  let rec parse_run machine { tokens; hypotheses } =
    match tokens with
    | t :: ts ->
        let new_state = parse_step machine t { tokens = ts; hypotheses } in
        parse_run machine new_state
    | [] -> (
        let accepting =
          ParseHypotheses.filter
            (fun (cfg, _) -> TaggedPda.is_accepting cfg)
            hypotheses
        in
        match ParseHypotheses.to_list accepting with
        | [] -> raise (ParseFail "no parse found")
        | [ (_, s) ] -> Gram.unwrap (ParseStack.unwrap s)
        | _ -> raise (ParseFail "ambiguous parse"))

  module Transition = struct
    type t =
      (terminal option * Gram.t)
      * (TaggedPda.state * Gram.t list * ActionRegistry.Tag.t)
    [@@deriving compare]
  end

  module TransitionSet = Set.Make (Transition)

  let nonterminal_to_transition nonterminal syms reduce state =
    let tag = register_prod syms reduce in
    ((None, N nonterminal), (state, syms, tag))

  let terminal_to_transition terminal shift state =
    let tag = register_cons shift in
    ((Some terminal, T terminal), (state, [], tag))

  let alt = TransitionSet.union
  let ( >>| ) = alt

  let rule_to_transition state = function
    | Production { lhs; rhss } ->
        let ts =
          List.map
            (fun { rhs; action } ->
              nonterminal_to_transition lhs rhs action state)
            rhss
        in
        TransitionSet.of_list ts
    | Consumption { lhs; action } ->
        TransitionSet.singleton (terminal_to_transition lhs action state)

  let compile grammar =
    let state = 0 in
    let ts =
      List.fold_right ( >>| )
        (List.map (rule_to_transition state) grammar)
        TransitionSet.empty
    in
    let transitions =
      TransitionSet.fold
        (fun (input, output) transitions ->
          let existing =
            match TaggedPda.Transition.find_opt input transitions with
            | Some outputs -> outputs
            | None -> TaggedPda.TransitionOutputSet.empty
          in
          TaggedPda.Transition.add input
            (TaggedPda.TransitionOutputSet.add output existing)
            transitions)
        ts TaggedPda.Transition.empty
    in
    TaggedPda.
      {
        states = TaggedPda.State.singleton state;
        initial_state = state;
        initial_stack_sym = N Gram.start;
        next = (fun _ -> transitions);
      }

  let start_rule =
    Production { lhs = Gram.start; rhss = [ Gram.start_production ] }

  let parser = compile (start_rule :: Gram.grammar)

  let parse tokens =
    let initial_hypothesis =
      ( TaggedPda.Config.
          {
            current_state = parser.TaggedPda.initial_state;
            stack = [ parser.TaggedPda.initial_stack_sym ];
          },
        ParseStack.empty )
    in
    let initial_state =
      {
        tokens = tokens @ [ Gram.eof ];
        hypotheses = ParseHypotheses.singleton initial_hypothesis;
      }
    in
    parse_run parser initial_state
end

module LL1 (Gram : Grammar.S) = struct
  open Gram

  type token = Gram.token
  type ast = Gram.ast

  module ReductionRegistry =
    Registry.Make
      (struct
        type t = Gram.reduce
      end)

  module ParseStack = struct
    type frame = {
      remaining : t list;
      collected : data list;
      action : ReductionRegistry.Tag.t;
    }
    [@@deriving compare]

    type t = frame list [@@deriving compare]

    let create_frame remaining action = { remaining; collected = []; action }
    let is_filled { remaining } = List.length remaining = 0

    let fill_frame { remaining; collected; action } data =
      match remaining with
      | r :: rs -> { remaining = rs; collected = data :: collected; action }
      | [] -> raise (ParseFail "already filled")

    let rec normalise = function
      | [] -> []
      | [ f ] -> [ f ]
      | f1 :: f2 :: fs as frames ->
          if is_filled f1 then
            let r = ReductionRegistry.get f1.action in
            let d = r (List.rev f1.collected) in
            normalise (fill_frame f2 d :: fs)
          else frames

    let empty = []

    let fill s d =
      match s with
      | [] -> raise (ParseFail "cannot fill an empty stack")
      | f :: fs -> normalise (fill_frame f d :: fs)

    let push s f = normalise (f :: s)

    let peek = function
      | { remaining } :: fs -> (
          match remaining with
          | [] -> raise (ParseFail "malformed stack")
          | sym :: syms -> sym)
      | [] -> raise (ParseFail "cannot peek empty stack")

    let unwrap = function
      | [] -> raise (ParseFail "cannot unwrap empty stack")
      | [ f ] ->
          if is_filled f then
            let r = ReductionRegistry.get f.action in
            r (List.rev f.collected)
          else raise (ParseFail "cannot unwrap partially filled frame")
      | _ -> raise (ParseFail "cannot unwrap non-singleton stack")
  end

  module ActionRegistry =
    Registry.Make
      (struct
        type t = token -> ParseStack.t -> ParseStack.t
      end)

  let prod_action syms reduce =
   fun _ s ->
    let f = ParseStack.create_frame syms reduce in
    ParseStack.push s f

  let cons_action shift = fun token s -> ParseStack.fill s (shift token)
  let register_cons shift = ActionRegistry.register (cons_action shift)

  let register_prod syms reduce =
    let tag = ReductionRegistry.register reduce in
    ActionRegistry.register (prod_action syms tag)

  module ParseTable = struct
    type key = Gram.t * Terminal.t
    type value = ActionRegistry.Tag.t
    type t = (key, value) Hashtbl.t

    let tbl : t = Hashtbl.create 128

    let find k =
      try Hashtbl.find tbl k
      with Not_found -> raise (ParseFail "key not found")

    let add k v =
      match Hashtbl.find tbl k with
      | v' ->
          if v <> v' then (
            match k with
            | T _, _ -> raise (ParseFail "duplicate consumption rule")
            | N _, _ -> raise (ParseFail "grammar not in LL1"))
      | exception Not_found -> Hashtbl.add tbl k v
  end

  open Analysis
  open GrammarAnalysis (Gram)

  type parsing_state = { tokens : token list; stack : ParseStack.t }

  let evolve_stack token tag stack =
    let action = ActionRegistry.get tag in
    action token stack

  let parse_step { tokens; stack } =
    match tokens with
    | [] -> raise (ParseFail "unexpected end of input")
    | tok :: toks -> (
        match ParseStack.peek stack with
        | T term ->
            let term' = Gram.token_to_terminal tok in
            if term = term' then
              {
                tokens = toks;
                stack = evolve_stack tok (ParseTable.find (T term, term)) stack;
              }
            else raise (ParseFail "parse fail")
        | N nonterm ->
            let term = Gram.token_to_terminal tok in
            {
              tokens;
              stack =
                evolve_stack tok (ParseTable.find (N nonterm, term)) stack;
            })

  let rec parse_run ({ tokens; stack } as state) =
    match tokens with
    | _ :: _ -> parse_run (parse_step state)
    | [] -> Gram.unwrap (ParseStack.unwrap stack)

  let compile grammar =
    let consumption_rules, production_rules =
      List.partition_map
        (function
          | Consumption { lhs; action } -> Left (lhs, action)
          | Production { lhs; rhss } -> Right (lhs, rhss))
        grammar
    in
    let start_tag =
      let { action; _ } = Gram.start_production in
      ReductionRegistry.register action
    in
    List.iter
      (fun (l, a) -> ParseTable.add (T l, l) (register_cons a))
      consumption_rules;
    List.iter
      (fun (lhs, rhss) ->
        List.iter
          (fun { rhs; action } ->
            let tag = register_prod rhs action in
            let firsts = First.syms rhs in
            let firsts' = unwrap firsts in
            TSet.iter (fun term -> ParseTable.add (N lhs, term) tag) firsts';
            if TESet.mem TE.Eps firsts then
              let follows = Follow.nonterminal lhs in
              TSet.iter (fun term -> ParseTable.add (N lhs, term) tag) follows)
          rhss)
      production_rules;
    start_tag

  let start_tag = compile Gram.grammar

  let parse tokens =
    let { rhs } = Gram.start_production in
    let frame = ParseStack.create_frame rhs start_tag in
    parse_run { tokens = tokens @ [ Gram.eof ]; stack = [ frame ] }
end

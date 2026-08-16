open Lang
open Ppx_compare_lib.Builtin

exception ParseFail of string

module General (Grammar : GRAMMAR) = struct
  module Elaborated = Elaborate (Grammar)
  open Elaborated
  module Bnf = Elaborated.Bnf
  open Bnf

  type token = Elaborated.token
  type ast = Elaborated.ast

  open Views (Bnf)

  module ParseStack = struct
    type frame = { production : production; data : data list; dot : int }
    [@@deriving compare]

    type t = frame list [@@deriving compare]

    let create_frame production = { production; data = []; dot = 0 }
    let is_filled { production; dot } = List.length production.rhs = dot

    let fill_frame { production; dot; data } datum =
      let suffix = List.drop dot production.rhs in
      match suffix with
      | _ :: _ -> { production; data = datum :: data; dot = dot + 1 }
      | [] -> raise (ParseFail "already filled")

    let rec normalise = function
      | [] -> []
      | [ f ] -> [ f ]
      | f1 :: f2 :: fs as frames ->
          if is_filled f1 then
            let d =
              build (builder_of_production f1.production) (List.rev f1.data)
            in
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
            build (builder_of_production f.production) (List.rev f.data)
          else raise (ParseFail "cannot unwrap partially filled frame")
      | _ -> raise (ParseFail "cannot unwrap non-singleton stack")
  end

  module StepRegistry = Registry.Make (struct
    type elt = token -> ParseStack.t -> ParseStack.t
  end)

  let predict_step production =
   fun _ s ->
    let f = ParseStack.create_frame production in
    ParseStack.push s f

  let register_prediction production =
    StepRegistry.register (predict_step production)

  let read_step r = fun token s -> ParseStack.fill s (read r token)
  let register_reader r = StepRegistry.register (read_step r)

  module TaggedPda = Automata.Tpda.Make (Terminal) (Sym) (StepRegistry.Id)

  module ParseHypothesis = struct
    type t = TaggedPda.config * ParseStack.t [@@deriving compare]
  end

  module ParseHypotheses = Set.Make (ParseHypothesis)

  type parsing_state = { tokens : token list; hypotheses : ParseHypotheses.t }

  let rec evolve_stack token tags stack =
    match tags with
    | [] -> stack
    | t :: ts ->
        let step = StepRegistry.get t in
        evolve_stack token ts (step token stack)

  let collect_traces token stack (new_cfg, trace) hypotheses =
    let new_stack = evolve_stack token trace stack in
    let hypothesis = (new_cfg, new_stack) in
    ParseHypotheses.add hypothesis hypotheses

  let advance_one machine token ((config, stack) : ParseHypothesis.t) =
    let traces =
      TaggedPda.consume machine
        (TaggedPda.TraceSet.singleton (config, []))
        (token_to_terminal token)
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
        | [ (_, s) ] -> finish (ParseStack.unwrap s)
        | _ -> raise (ParseFail "ambiguous parse"))

  module Transition = struct
    type t =
      (terminal option * Sym.t)
      * (TaggedPda.state * Sym.t list * StepRegistry.Id.t)
    [@@deriving compare]
  end

  module TransitionSet = Set.Make (Transition)

  let production_to_transition state (p : production) =
    let tag = register_prediction p in
    ((None, N p.lhs), (state, p.rhs, tag))

  let terminal_to_transition state terminal =
    let tag = register_reader (reader_of_terminal terminal) in
    ((Some terminal, T terminal), (state, [], tag))

  let alt = TransitionSet.union
  let ( >>| ) = alt

  let compile productions =
    let state = 0 in
    let ts =
      TransitionSet.of_list
        (List.map (production_to_transition state) productions)
      >>| TransitionSet.of_list
            (List.map (terminal_to_transition state) terminals)
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
        initial_stack_sym = N start;
        next = (fun _ -> transitions);
      }

  let parser = compile all_productions

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
        tokens = tokens @ [ eof ];
        hypotheses = ParseHypotheses.singleton initial_hypothesis;
      }
    in
    parse_run parser initial_state
end

module LL1 (Grammar : GRAMMAR) = struct
  module Elaborated = Elaborate (Grammar)
  open Elaborated
  module Bnf = Elaborated.Bnf
  open Bnf

  type token = Elaborated.token
  type ast = Elaborated.ast

  open Views (Bnf)

  module ParseStack = struct
    type frame = { production : production; data : data list; dot : int }
    [@@deriving compare]

    type t = frame list [@@deriving compare]

    let create_frame production = { production; data = []; dot = 0 }
    let is_filled { production; dot } = List.length production.rhs = dot

    let fill_frame { production; dot; data } datum =
      let suffix = List.drop dot production.rhs in
      match suffix with
      | _ :: _ -> { production; data = datum :: data; dot = dot + 1 }
      | [] -> raise (ParseFail "already filled")

    let rec normalise = function
      | [] -> []
      | [ f ] -> [ f ]
      | f1 :: f2 :: fs as frames ->
          if is_filled f1 then
            let d =
              build (builder_of_production f1.production) (List.rev f1.data)
            in
            normalise (fill_frame f2 d :: fs)
          else frames

    let empty = []

    let fill s d =
      match s with
      | [] -> raise (ParseFail "cannot fill an empty stack")
      | f :: fs -> normalise (fill_frame f d :: fs)

    let push s f = normalise (f :: s)

    let peek = function
      | { production; dot; _ } :: _ -> (
          match List.drop dot production.rhs with
          | [] -> raise (ParseFail "malformed stack")
          | sym :: _ -> sym)
      | [] -> raise (ParseFail "cannot peek empty stack")

    let unwrap = function
      | [] -> raise (ParseFail "cannot unwrap empty stack")
      | [ f ] ->
          if is_filled f then
            build (builder_of_production f.production) (List.rev f.data)
          else raise (ParseFail "cannot unwrap partially filled frame")
      | _ -> raise (ParseFail "cannot unwrap non-singleton stack")
  end

  module StepRegistry = Registry.Make (struct
    type elt = token -> ParseStack.t -> ParseStack.t
  end)

  let predict_step production =
   fun _ s ->
    let f = ParseStack.create_frame production in
    ParseStack.push s f

  let register_prediction production =
    StepRegistry.register (predict_step production)

  let read_step r = fun token s -> ParseStack.fill s (read r token)
  let register_reader r = StepRegistry.register (read_step r)

  module ParseTable = struct
    type key = Sym.t * Terminal.t
    type value = StepRegistry.Id.t
    type t = (key, value) Hashtbl.t

    let tbl : t = Hashtbl.create 128

    let find k =
      try Hashtbl.find tbl k
      with Not_found -> raise (ParseFail "key not found")

    let add k v =
      match Hashtbl.find tbl k with
      | v' -> (
          if v <> v' then
            match k with
            | T _, _ -> raise (ParseFail "duplicate consumption")
            | N _, _ -> raise (ParseFail "grammar not in LL1"))
      | exception Not_found -> Hashtbl.add tbl k v
  end

  open Analysis
  open GrammarAnalysis (Bnf)

  type parsing_state = { tokens : token list; stack : ParseStack.t }

  let evolve_stack token tag stack =
    let step = StepRegistry.get tag in
    step token stack

  let parse_step { tokens; stack } =
    match tokens with
    | [] -> raise (ParseFail "unexpected end of input")
    | tok :: toks -> (
        match ParseStack.peek stack with
        | T term ->
            let term' = token_to_terminal tok in
            if term = term' then
              {
                tokens = toks;
                stack = evolve_stack tok (ParseTable.find (T term, term)) stack;
              }
            else raise (ParseFail "parse fail")
        | N nonterm ->
            let term = token_to_terminal tok in
            {
              tokens;
              stack = evolve_stack tok (ParseTable.find (N nonterm, term)) stack;
            })

  let rec parse_run ({ tokens; stack } as state) =
    match tokens with
    | _ :: _ -> parse_run (parse_step state)
    | [] -> Elaborated.finish (ParseStack.unwrap stack)

  let compile ps =
    List.iter
      (fun terminal ->
        ParseTable.add (T terminal, terminal)
          (register_reader (reader_of_terminal terminal)))
      terminals;
    List.iter
      (fun (p : production) ->
        let tag = register_prediction p in
        let firsts = First.syms p.rhs in
        let firsts' = drop_eps firsts in
        TSet.iter (fun term -> ParseTable.add (N p.lhs, term) tag) firsts';
        if TESet.mem TE.Eps firsts then
          let follows = Follow.nonterminal p.lhs in
          TSet.iter (fun term -> ParseTable.add (N p.lhs, term) tag) follows)
      ps

  let () = compile productions.rest

  let parse tokens =
    let frame = ParseStack.create_frame productions.start in
    parse_run { tokens = tokens @ [ eof ]; stack = [ frame ] }
end

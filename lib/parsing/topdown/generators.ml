open Lang
open Ppx_compare_lib.Builtin

exception Parse_error of string

module Generalised (Grammar : GRAMMAR) = struct
  module Augmented = Topdown_augment (Grammar)
  open Augmented
  module Bnf = Augmented.Bnf
  open Bnf

  type token = Augmented.token
  type ast = Augmented.ast

  open Views (Bnf)

  module Parse_stack = struct
    type frame = { production : production; data : data list; dot : int }
    [@@deriving compare]

    type t = frame list [@@deriving compare]

    let create_frame production = { production; data = []; dot = 0 }
    let is_filled { production; dot } = List.length production.rhs = dot

    let fill_frame { production; dot; data } datum =
      let suffix = List.drop dot production.rhs in
      match suffix with
      | _ :: _ -> { production; data = datum :: data; dot = dot + 1 }
      | [] -> raise (Parse_error "already filled")

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
      | [] -> raise (Parse_error "cannot fill an empty stack")
      | f :: fs -> normalise (fill_frame f d :: fs)

    let push s f = normalise (f :: s)

    let unwrap = function
      | [] -> raise (Parse_error "cannot unwrap empty stack")
      | [ f ] ->
          if is_filled f then
            build (builder_of_production f.production) (List.rev f.data)
          else raise (Parse_error "cannot unwrap partially filled frame")
      | _ -> raise (Parse_error "cannot unwrap non-singleton stack")
  end

  module Step_registry = Registry.Make (struct
    type elt = token -> Parse_stack.t -> Parse_stack.t
  end)

  let predict_step production =
   fun _ s ->
    let f = Parse_stack.create_frame production in
    Parse_stack.push s f

  let register_prediction production =
    Step_registry.register (predict_step production)

  let read_step r = fun token s -> Parse_stack.fill s (read r token)
  let register_reader r = Step_registry.register (read_step r)

  module Tagged_pda = Automata.Tpda.Make (Terminal) (Sym) (Step_registry.Id)

  module Parse_hypothesis = struct
    type t = Tagged_pda.config * Parse_stack.t [@@deriving compare]
  end

  module Parse_hypothesis_set = Set.Make (Parse_hypothesis)

  type parse_state = {
    tokens : token list;
    hypotheses : Parse_hypothesis_set.t;
  }

  let rec evolve_stack token tags stack =
    match tags with
    | [] -> stack
    | t :: ts ->
        let step = Step_registry.get t in
        evolve_stack token ts (step token stack)

  let collect_traces token stack (new_cfg, trace) hypotheses =
    let new_stack = evolve_stack token trace stack in
    let hypothesis = (new_cfg, new_stack) in
    Parse_hypothesis_set.add hypothesis hypotheses

  let advance_one machine token ((config, stack) : Parse_hypothesis.t) =
    let traces =
      Tagged_pda.consume machine
        (Tagged_pda.Trace_set.singleton (config, []))
        (token_to_terminal token)
    in
    Tagged_pda.Trace_set.fold
      (collect_traces token stack)
      traces Parse_hypothesis_set.empty

  let advance machine token hypotheses =
    Parse_hypothesis_set.fold
      (fun hyp ->
        fun hyps ->
         let hyps' = advance_one machine token hyp in
         Parse_hypothesis_set.union hyps hyps')
      hypotheses Parse_hypothesis_set.empty

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
          Parse_hypothesis_set.filter
            (fun (cfg, _) -> Tagged_pda.is_accepting cfg)
            hypotheses
        in
        match Parse_hypothesis_set.to_list accepting with
        | [] -> raise (Parse_error "no parse found")
        | [ (_, s) ] -> finish (Parse_stack.unwrap s)
        | _ -> raise (Parse_error "ambiguous parse"))

  module Transition = struct
    type t =
      (terminal option * Sym.t)
      * (Tagged_pda.state * Sym.t list * Step_registry.Id.t)
    [@@deriving compare]
  end

  module Transition_set = Set.Make (Transition)

  let production_to_transition state (p : production) =
    let tag = register_prediction p in
    ((None, N p.lhs), (state, p.rhs, tag))

  let terminal_to_transition state terminal =
    let tag = register_reader (reader_of_terminal terminal) in
    ((Some terminal, T terminal), (state, [], tag))

  let alt = Transition_set.union
  let ( <|> ) = alt

  let compile productions =
    let state = 0 in
    let ts =
      Transition_set.of_list
        (List.map (production_to_transition state) productions)
      <|> Transition_set.of_list
            (List.map (terminal_to_transition state) terminals)
    in
    let transitions =
      Transition_set.fold
        (fun (input, output) transitions ->
          let existing =
            match Tagged_pda.Transition_map.find_opt input transitions with
            | Some outputs -> outputs
            | None -> Tagged_pda.Transition_output_set.empty
          in
          Tagged_pda.Transition_map.add input
            (Tagged_pda.Transition_output_set.add output existing)
            transitions)
        ts Tagged_pda.Transition_map.empty
    in
    Tagged_pda.
      {
        states = Tagged_pda.State_set.singleton state;
        initial_state = state;
        initial_stack_sym = N start;
        next = (fun _ -> transitions);
      }

  let parser = compile all_productions

  let parse tokens =
    let initial_hypothesis =
      ( Tagged_pda.Config.
          {
            current_state = parser.Tagged_pda.initial_state;
            stack = [ parser.Tagged_pda.initial_stack_sym ];
          },
        Parse_stack.empty )
    in
    let initial_state =
      {
        tokens = tokens @ [ eof ];
        hypotheses = Parse_hypothesis_set.singleton initial_hypothesis;
      }
    in
    parse_run parser initial_state
end

module Ll1 (Grammar : GRAMMAR) = struct
  module Augmented = Topdown_augment (Grammar)
  open Augmented
  module Bnf = Augmented.Bnf
  open Bnf

  type token = Augmented.token
  type ast = Augmented.ast

  open Views (Bnf)

  module Parse_stack = struct
    type frame = { production : production; data : data list; dot : int }
    [@@deriving compare]

    type t = frame list [@@deriving compare]

    let create_frame production = { production; data = []; dot = 0 }
    let is_filled { production; dot } = List.length production.rhs = dot

    let fill_frame { production; dot; data } datum =
      let suffix = List.drop dot production.rhs in
      match suffix with
      | _ :: _ -> { production; data = datum :: data; dot = dot + 1 }
      | [] -> raise (Parse_error "already filled")

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
      | [] -> raise (Parse_error "cannot fill an empty stack")
      | f :: fs -> normalise (fill_frame f d :: fs)

    let push s f = normalise (f :: s)

    let peek = function
      | { production; dot; _ } :: _ -> (
          match List.drop dot production.rhs with
          | [] -> raise (Parse_error "malformed stack")
          | sym :: _ -> sym)
      | [] -> raise (Parse_error "cannot peek empty stack")

    let unwrap = function
      | [] -> raise (Parse_error "cannot unwrap empty stack")
      | [ f ] ->
          if is_filled f then
            build (builder_of_production f.production) (List.rev f.data)
          else raise (Parse_error "cannot unwrap partially filled frame")
      | _ -> raise (Parse_error "cannot unwrap non-singleton stack")
  end

  module Step_registry = Registry.Make (struct
    type elt = token -> Parse_stack.t -> Parse_stack.t
  end)

  let predict_step production =
   fun _ s ->
    let f = Parse_stack.create_frame production in
    Parse_stack.push s f

  let register_prediction production =
    Step_registry.register (predict_step production)

  let read_step r = fun token s -> Parse_stack.fill s (read r token)
  let register_reader r = Step_registry.register (read_step r)

  module Parse_table = struct
    type key = Sym.t * Terminal.t
    type value = Step_registry.Id.t
    type t = (key, value) Hashtbl.t

    let tbl : t = Hashtbl.create 128

    let find k =
      try Hashtbl.find tbl k
      with Not_found -> raise (Parse_error "key not found")

    let add k v =
      match Hashtbl.find tbl k with
      | v' -> (
          if v <> v' then
            match k with
            | T _, _ -> raise (Parse_error "duplicate consumption")
            | N _, _ -> raise (Parse_error "grammar not in Ll1"))
      | exception Not_found -> Hashtbl.add tbl k v
  end

  open Analysis
  open Grammar_analysis (Bnf)

  type parse_state = { tokens : token list; stack : Parse_stack.t }

  let evolve_stack token tag stack =
    let step = Step_registry.get tag in
    step token stack

  let parse_step { tokens; stack } =
    match tokens with
    | [] -> raise (Parse_error "unexpected end of input")
    | tok :: toks -> (
        match Parse_stack.peek stack with
        | T term ->
            let term' = token_to_terminal tok in
            if term = term' then
              {
                tokens = toks;
                stack = evolve_stack tok (Parse_table.find (T term, term)) stack;
              }
            else raise (Parse_error "parse fail")
        | N nonterm ->
            let term = token_to_terminal tok in
            {
              tokens;
              stack =
                evolve_stack tok (Parse_table.find (N nonterm, term)) stack;
            })

  let rec parse_run ({ tokens; stack } as state) =
    match tokens with
    | _ :: _ -> parse_run (parse_step state)
    | [] -> Augmented.finish (Parse_stack.unwrap stack)

  let compile ps =
    List.iter
      (fun terminal ->
        Parse_table.add (T terminal, terminal)
          (register_reader (reader_of_terminal terminal)))
      terminals;
    List.iter
      (fun (p : production) ->
        let tag = register_prediction p in
        let firsts = First.syms p.rhs in
        let firsts' = to_terminals firsts in
        Terminal_set.iter
          (fun term -> Parse_table.add (N p.lhs, term) tag)
          firsts';
        if Term_or_eps_set.mem Term_or_eps.Eps firsts then
          let follows = Follow.nonterminal p.lhs in
          Terminal_set.iter
            (fun term -> Parse_table.add (N p.lhs, term) tag)
            follows)
      ps

  let () = compile productions.rest

  let parse tokens =
    let frame = Parse_stack.create_frame productions.start in
    parse_run { tokens = tokens @ [ eof ]; stack = [ frame ] }
end

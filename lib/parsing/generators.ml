open Intfs
open Ppx_compare_lib.Builtin

exception ParseFail of string

module General (Gram : Grammar.S) = struct
  open Gram

  type token = Gram.token
  type ast = Gram.ast

  module Reductions = struct
    type id = int

    let compare_id = Int.compare
    let reduce_tbl : (id, reduce) Hashtbl.t = Hashtbl.create 32
    let id_ref = ref 0

    let id_gen () =
      let x = !id_ref in
      id_ref := x + 1;
      x

    let register reduction =
      let new_id = id_gen () in
      Hashtbl.add reduce_tbl new_id reduction;
      new_id

    let find id = Hashtbl.find reduce_tbl id
  end

  module ParseStack = struct
    type frame = {
      remaining : t list;
      collected : data list;
      action : Reductions.id;
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
            let r = Reductions.find f1.action in
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
            let r = Reductions.find f.action in
            r (List.rev f.collected)
          else raise (ParseFail "cannot unwrap partially filled frame")
      | _ -> raise (ParseFail "cannot unwrap non-singleton stack")
  end

  module ParseTag = struct
    type t = Match of terminal | Predict of Reductions.id [@@deriving compare]
    type action = token -> ParseStack.t -> ParseStack.t

    let prod_action production reduce =
     fun _token ->
      fun s ->
       let f = ParseStack.create_frame production reduce in
       ParseStack.push s f

    let cons_action shift =
     fun token -> fun s -> ParseStack.fill s (shift token)

    let tag_to_action_tbl : (t, action) Hashtbl.t = Hashtbl.create 32

    let register_cons terminal shift =
      let tag = Match terminal in
      let act = cons_action shift in
      Hashtbl.add tag_to_action_tbl tag act;
      tag

    let register_prod production reduce =
      let prod_id = Reductions.register reduce in
      let tag = Predict prod_id in
      let act = prod_action production prod_id in
      Hashtbl.add tag_to_action_tbl tag act;
      tag

    let tag_to_action = Hashtbl.find tag_to_action_tbl
  end

  module TaggedPda = Tpda.Make (Terminal) (Gram) (ParseTag)

  module ParseHypothesis = struct
    type t = TaggedPda.config * ParseStack.t [@@deriving compare]
  end

  module ParseHypotheses = Set.Make (ParseHypothesis)

  type parsing_state = { tokens : token list; hypotheses : ParseHypotheses.t }

  let rec evolve_stack token tags stack =
    match tags with
    | [] -> stack
    | t :: ts ->
        let action = ParseTag.tag_to_action t in
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
      (terminal option * Gram.t) * (TaggedPda.state * Gram.t list * ParseTag.t)
    [@@deriving compare]
  end

  module TransitionSet = Set.Make (Transition)

  let nonterminal_to_transition nonterminal production reduce state =
    let tag = ParseTag.register_prod production reduce in
    ((None, N nonterminal), (state, production, tag))

  let terminal_to_transition terminal shift state =
    let tag = ParseTag.register_cons terminal shift in
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
        (fun (input, output) ->
          fun transitions ->
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

  let parser = compile Gram.grammar

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
      { tokens; hypotheses = ParseHypotheses.singleton initial_hypothesis }
    in
    parse_run parser initial_state
end

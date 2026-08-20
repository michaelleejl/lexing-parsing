open Lang
open States

module Actions (State : STATE) = struct
  open State
  module Item = State.Item
  open Item
  open Views (Item)
  module ItemSet = State.ItemSet

  type act = Shift | Reduce of production

  let table = Hashtbl.create 256

  type action_table = (State.t * terminal, act option) Hashtbl.t

  let act item terminal =
    match next item with
    | None ->
        if is_valid_for item terminal then Some (Reduce (production_of item))
        else None
    | Some (T t) -> if t <> terminal then None else Some Shift
    | _ -> None

  let action state terminal =
    let items = items_of state in
    let acts =
      ItemSet.fold
        (fun item acc ->
          match (act item terminal, acc) with
          | None, acc -> acc
          | Some a, acc -> a::acc)
        items []
    in
    acts

  let add state terminal =
    Hashtbl.add table (state, terminal) (action state terminal)
  ;;

  List.iter (fun s -> List.iter (add s) terminals) all_states;;

  let find state terminal =
    Hashtbl.find table (state, terminal)
end


module Action (State : STATE) = struct
  open State
  module Item = State.Item
  open Item
  open Views (Item)
  module ItemSet = State.ItemSet

  exception Conflict
  exception NoAction

  type act = Shift | Reduce of production 

  let table = Hashtbl.create 256

  type action_table = (State.t * terminal, act option) Hashtbl.t

  let act item terminal =
    match next item with
    | None ->
        if is_valid_for item terminal then Some (Reduce (production_of item))
        else None
    | Some (T t) -> if t <> terminal then None else Some Shift
    | _ -> None

  let action state terminal =
    let items = items_of state in
    let act_opt =
      ItemSet.fold
        (fun item acc ->
          match (act item terminal, acc) with
          | None, acc -> acc
          | Some Shift, Some Shift -> Some Shift
          | a, None -> a
          | _ -> raise Conflict)
        items None
    in
    act_opt

  let add state terminal =
    Hashtbl.add table (state, terminal) (action state terminal)
  ;;

  List.iter (fun s -> List.iter (add s) terminals) all_states;;

  let find state terminal =
    match Hashtbl.find table (state, terminal) with
    | None -> raise NoAction
    | Some a -> a
end

module Goto (State : STATE) = struct
  open State
  module Item = State.Item
  open Item
  open Views (Item)
  module ItemSet = State.ItemSet

  type goto_table = (State.t * nonterminal, State.t) Hashtbl.t

  let table = Hashtbl.create 256

  let add state nonterminal =
    Hashtbl.add table (state, nonterminal) (State.next state (N nonterminal))
  ;;

  List.iter (fun s -> List.iter (add s) nonterminals) all_states;;

  let find state nonterminal = Hashtbl.find table (state, nonterminal)
end

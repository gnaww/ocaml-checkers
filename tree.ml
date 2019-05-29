open State

(** The abstract type representing the game tree where the root represents the 
    current state of the board, the children of root represent the list of
    possible moves from the current board state and the associated heuristic
    value for the strength of the AI's position after making the move. The 
    second depth of the tree represents the list of all possible moves the 
    player can make in response to the AI and the associated heuristic value 
    for the strength of the AI's position afterward the player's move.
*)
type tree = Root of int * tree list | Node of (int * bool * coordinate * coordinate) * tree list

(** [all_piece state] is the coordinate list of the coordinates of all pieces on 
    the board of the color of the current player's turn in state [state]. *)
let all_piece state : coordinate list =
  let board = get_board state in 
  let rec all_piece_help board lst : coordinate list = 
    match board with 
    | [] -> lst 
    | h::t -> 
      if (snd h).color = get_turn_color state
      then all_piece_help t ((fst h)::lst)
      else all_piece_help t lst
  in all_piece_help board []

(** [add_coord_bool b coord lst] is a list of triples constructed from [lst] 
    with boolean [b] and coordinate [coord] added to each index as the first 
    and second element of the triple. Boolean [b] is true if the coordinate
    represents a move and false if the coordinates represents a capture. *)
let add_coord_bool b coord lst = 
  let rec add_coord_bool_help b coord lst acc = 
    match lst with 
    | [] -> acc
    | h::t -> 
      add_coord_bool_help b coord t ((b, coord, h) :: acc)
  in add_coord_bool_help b coord lst []


(** [all_moves state all_pieces] is a list of triples containing all the 
    possible moves in state [state] given all the piece coordinates 
    [all_pieces] of the current turn color pieces, where the first element in 
    the triple is a boolean representing the type of move 
    (true = move, false = capture), and the second and third elements are the 
    coordinate-from and coordinate-to, repsectively. *)
let all_moves state all_pieces : (bool * coordinate * coordinate) list = 
  let rec all_moves_help state all_pieces acc mustcap = 
    match all_pieces with 
    | [] -> if mustcap = true then List.filter (fun (b, _, _) -> not b) acc else acc
    | h::t ->
      let moves_captures = list_moves h state in
      let moves = fst moves_captures in
      let captures = snd moves_captures in
      let coord_bool_moves = add_coord_bool true h moves in 
      let coord_bool_captures = add_coord_bool false h captures in
      if List.length captures > 0 && mustcap = false then
        all_moves_help state t (coord_bool_moves @ coord_bool_captures @ acc) true
      else all_moves_help state t (coord_bool_moves @ coord_bool_captures @ acc) mustcap 
  in all_moves_help state all_pieces [] false

(** [calc_distance aipiece playerpiece] is the number of (integer) moves it 
    takes to move from the coordinate [aipiece] to coordinate [playerpiece] 
    on a normal game baord. *)
let calc_distance aipiece playerpiece = 
  let aipiece_int = int_of_string (snd aipiece) in
  let playerpiece_int = int_of_string (snd playerpiece) in
  Pervasives.abs (aipiece_int - playerpiece_int)

(** [min_dist_coord coord_piece_list] is the coordinate with the lowest 
    associated distance argument from the tuple list [coord_piece_list] *)
let min_dist_coord coord_piece_list = 
  let rec mdc_help curr_min coord = function 
    | [] -> coord 
    | h::t -> if (snd h) < curr_min then mdc_help (snd h) (fst h) t
      else mdc_help curr_min coord t
  in mdc_help 10 ("z", "-1") coord_piece_list

(** [nearest_opp state coord_aiking] is the coordinate of the nearest opposing
    pawn to coordinate [coord_aiking] in state [state]. If no opposing pawns 
    are in play, is the coordinate of the nearest opposing king. *)
let nearest_opp state coord_aiking = 
  let red_pawns = List.filter (fun (coord, piece) -> 
      piece.king = false && piece.color = "red") (snd state) in
  if List.length red_pawns <> 0 then
    let red_pawns_distance = List.map (fun (coord, piece) -> 
        (coord, calc_distance coord_aiking coord)) red_pawns in
    min_dist_coord (red_pawns_distance)
  else let red_kings = List.filter (fun (coord, piece) -> 
      piece.king = true && piece.color = "red") (snd state) in
    let red_kings_distance = List.map (fun (coord, piece) -> 
        (coord, calc_distance coord_aiking coord)) red_kings in
    min_dist_coord (red_kings_distance)

(** [nearest_king state coord_aiking] is the coordinate of the nearest friendly
    king to coordinate [coord_aiking] in state [state]. If no other kings 
    are in play, is dummy (non-existant) coordinate ("z", "-1"). *)
let nearest_king state coord_aiking = 
  let white_kings = List.filter (fun (coord, piece) -> 
      piece.king = true && piece.color = "white") (snd state) in
  if List.length white_kings <> 0 then
    let white_kings_distance = List.map (fun (coord, piece) -> 
        (coord, calc_distance coord_aiking coord)) white_kings in
    min_dist_coord (white_kings_distance)
  else ("z", "-1")

(** [get_value state] is sum value of the all pieces on the board of the 
    current player's turn in state [state]. Values are allocated as follows: 
    KINGS = 20 points with associated deductions
    -1 for each move away from the nearest opponent
    -1 for each move away from the nearest friendly king
    PAWNS = 5 points with associated additions
    +1 for each square closer to the opposite end of the board
    +2 for being on an edge square
    +2 for being in the back row
    Edge and back row weights are reduced to +1 when there are less than 10
    pieces left and the the game has progressed to late game stages
*)
let get_value state : int = 
  let board = get_board state in 
  let rec get_value_help board acc = 
    match board with 
    | [] -> acc
    | h::t -> 
      if (snd h).color = get_turn_color state then 
        let edge_weight = (if List.length (snd state) > 10 then 2 else 1) in
        let back_row_weight = (if List.length (snd state) > 10 then 2 else 1) in
        if (snd h).king then
          let king_coord = (fst h) in 
          let nearest_opp = nearest_opp state king_coord in 
          let nearest_king = nearest_king state king_coord in
          if nearest_king <> ("z", "-1") then  
            let opp_distance = calc_distance nearest_opp king_coord in 
            let king_distance = calc_distance nearest_king king_coord in 
            get_value_help t (20 - opp_distance - king_distance + acc)
          else 
            let opp_distance = calc_distance nearest_opp king_coord in
            get_value_help t (20 - opp_distance + acc)
        else if ((fst (fst h)) = "a") && ((snd (fst h)) = "8") &&
                get_turn_color state = "white" then 
          let coord_num = 8 - (int_of_string (snd (fst h))) in
          get_value_help t (5 + coord_num + edge_weight + back_row_weight + acc)
        else if ((fst (fst h)) = "h") && ((snd (fst h)) = "1") &&
                get_turn_color state = "red" then
          let coord_num = 8 - (int_of_string (snd (fst h))) in
          get_value_help t (5 + coord_num + edge_weight + back_row_weight + acc)
        else if ((snd (fst h)) = "8") && get_turn_color state = "white" then 
          let coord_num = 8 - (int_of_string (snd (fst h))) in
          get_value_help t (5 + coord_num + back_row_weight + acc)
        else if ((snd (fst h)) = "1") && get_turn_color state = "red" then 
          let coord_num = 8 - (int_of_string (snd (fst h))) in
          get_value_help t (5 + coord_num + back_row_weight + acc)
        else if (fst (fst h)) = "a" || (fst (fst h)) = "h" then 
          let coord_num = 8 - (int_of_string (snd (fst h))) in
          get_value_help t (5 + coord_num + edge_weight + acc)
        else
          let coord_num = 8 - (int_of_string (snd (fst h))) in
          get_value_help t (5 + coord_num + acc)
      else get_value_help t acc
  in get_value_help board 0

(** [make_nodes state] is a tree list representing all of the possible moves
    from state [state], each with the associated board value from [get_value] *)
let rec make_nodes state  =
  let all_pieces = all_piece state in 
  let all_moves = all_moves state all_pieces
  in make_nodes_help all_moves state [] 

and make_nodes_help all_moves state acc : tree list = 
  match all_moves with 
  | [] -> acc
  | (b, coord_from, coord_to)::t -> 
    if b = true then
      let new_state = move coord_from coord_to state in 
      let value = get_value new_state in
      make_nodes_help t state (Node ((value, b, coord_from, coord_to), []) :: acc)
    else
      let new_state = capture coord_from coord_to state in 
      let value = get_value new_state in
      make_nodes_help t state (Node ((value, b, coord_from, coord_to), []) :: acc)

(** [make_tree state] is a tree with a root, representing the current 
    state [state], a depth 1 tree list representing all of the possible moves
    and associated board values from [make_nodes state], and a depth 2 tree
    list that gives the list of all possible moves and associated board values
    from each depth 1 board state node. *)
let make_tree state = 
  let depth1 = make_nodes state in
  let rec make_tree_help depth acc = 
    match depth with 
    | [] -> Root (0, acc)
    | Node ((i, b, cf, ct), tl)::t -> 
      if b = true 
      then let new_state = move cf ct state in 
        let nodes = make_nodes new_state in 
        make_tree_help t ((Node ((i, b, cf, ct), nodes))::acc)
      else let new_state  = capture cf ct state in 
        let nodes = make_nodes new_state in 
        make_tree_help t (Node ((i, b, cf, ct), nodes)::acc)
    | _ -> failwith "should be a Node"
  in make_tree_help depth1 []

(** [min_list treelist] is the integer representing the minimum board value
    from tree list [treelist], considering only the nodes at depth 1. *)
let min_list treelist = 
  let rec min_list_help treelist min = 
    match treelist with 
    | [] -> min
    | Node ((i, _, _, _), _)::t ->
      if i < min
      then min_list_help t i
      else min_list_help t min
    | _ -> failwith "should be a Node"    
  in min_list_help treelist 180

(** [max_value node] is an integer calculated by adding the value of node [node] 
    to the minimum value child node in that node's associated child tree list. *)
let max_value node = 
  match node with
  | Node ((i, b, cf, ct), tl) ->
    if b = true then 
      i + (min_list tl)
    else 
      i + (min_list tl)
  | _ -> failwith "should be a Node"

(** [best_move_val root] is a tuple where the first entry is the integer value 
    from the maximum valued node of the tree branching from root [root], 
    calculated by taking the [max_value] of each depth 1 node. The second value
    of the tuple is a triple representing the depth 1 move associated with
    the maximum valued node, where the first element is b = true for a move 
    b = false for a capture, and the second and third elements are the 
    coordinate-from and coordinate-to, respectively. *)
let best_move_val root =
  match root with 
  | Root (_, treelist) ->
    let rec bmv_help treelist max b cf ct =
      match treelist with 
      | [] -> (max, (b, cf, ct))
      | Node ((i, b1, cf1, ct1), tl)::t -> 
        let value = max_value (Node ((i, b1, cf1, ct1), tl)) in
        if value > max then bmv_help t value b1 cf1 ct1
        else bmv_help t max b cf ct
      | _ -> failwith "should be a Node"
    in bmv_help treelist (-180) true ("z", "-1") ("z", "-1") 
  | _ -> failwith "this should be a Root"

(** [best_move state] is the tuple where the the second element is the current 
    state [state], and first element is a tuple of the best move type 
    (true = move, false = capture) and the best move coordinate-to. *)
let best_move state = 
  let root = make_tree state in 
  let (b, cf, ct) = snd (best_move_val root) in
  if b = true  
  then ((b, ct), move cf ct state) 
  else ((b, ct), capture cf ct state)

(** [hint state] is the tuple of the best move coordinate-from 
    and best move coordinate-to in state [state]. *)
let hint state =
  let root = make_tree state in
  let (_, cf, ct) = snd (best_move_val root) in
  cf, ct












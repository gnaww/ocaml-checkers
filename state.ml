(* State *)
exception NoPiece
exception FullSquare
exception Backwards
exception NotAdjacent
exception NoPieceToCapture
exception NotAdjCapture
exception OwnPieceCap
exception MustCap

type piece = {
  color: string;
  king: bool;
}

type coordinate = string * string

type board = (coordinate * piece) list

type state = (bool * board)

let init_board =
  [(("b", "1"), {color = "red"; king = false});
   (("d", "1"), {color = "red"; king = false});
   (("f", "1"), {color = "red"; king = false});
   (("h", "1"), {color = "red"; king = false});
   (("a", "2"), {color = "red"; king = false});
   (("c", "2"), {color = "red"; king = false});
   (("e", "2"), {color = "red"; king = false});
   (("g", "2"), {color = "red"; king = false});
   (("b", "3"), {color = "red"; king = false});
   (("d", "3"), {color = "red"; king = false});
   (("f", "3"), {color = "red"; king = false});
   (("h", "3"), {color = "red"; king = false});
   (("a", "6"), {color = "white"; king = false});
   (("c", "6"), {color = "white"; king = false});
   (("e", "6"), {color = "white"; king = false});
   (("g", "6"), {color = "white"; king = false}); 
   (("b", "7"), {color = "white"; king = false});
   (("d", "7"), {color = "white"; king = false});
   (("f", "7"), {color = "white"; king = false});
   (("h", "7"), {color = "white"; king = false});
   (("a", "8"), {color = "white"; king = false});
   (("c", "8"), {color = "white"; king = false});
   (("e", "8"), {color = "white"; king = false});
   (("g", "8"), {color = "white"; king = false});
  ]

let init_state = (true, init_board)

let pos_to_piece coordinate board : piece =
  let element = List.find (fun x -> fst x = coordinate) board in 
  snd element

let get_color coord (board : board) =
  let piece = pos_to_piece coord board in
  piece.color

let is_king coord board =
  let piece = pos_to_piece coord board in
  piece.king

(** [get_turn state] is the boolean that represents the turn. *)
let get_turn state = fst state

let get_turn_color state = if get_turn state = true then "red" else "white"

let get_board state : board = snd state

let is_empty coord board = 
  not (List.exists (fun x -> fst x = coord) board)

let noPiece (coord_from : coordinate) (state : state) =
  try
    if (is_empty coord_from (get_board state)) || 
       (get_color coord_from (get_board state)) <> (get_turn_color state)
    then raise NoPiece else true
  with Not_found -> raise NoPiece

let fullSquare coord_to board = 
  if (not (is_empty coord_to board))
  then raise FullSquare else true

let backwards color (coord_from : coordinate) (coord_to : coordinate) 
    (board : board) = 
  if (is_king coord_from board) then true else 
    let from_int = int_of_string (snd coord_from) in 
    let to_int = int_of_string (snd coord_to) in 
    if ((color = "white") && (to_int > from_int)) || 
       ((color = "red") && to_int < from_int) 
    then raise Backwards else true

let notAdjacent (coord_from : coordinate) (coord_to : coordinate) = 
  let from_int = int_of_string (snd coord_from) in 
  let to_int = int_of_string (snd coord_to) in 
  let char = String.get (fst coord_from) 0 in
  let inc = Char.chr (Char.code char + 1) in 
  let dec = Char.chr (Char.code char - 1) in 
  if (fst coord_to = Char.escaped inc || fst coord_to = Char.escaped dec) &&
     (fst coord_to <> "i" && fst coord_to <> "`") && 
     (from_int + 1 = to_int || from_int - 1 = to_int)
     && (to_int <> 0 && to_int <> 9)
  then true else raise NotAdjacent

let valid_move (coord_from : coordinate) (coord_to : coordinate) state =
  try
    let board = get_board state in 
    let color = get_turn_color state in 
    noPiece coord_from state &&
    fullSquare coord_to board &&
    backwards color coord_from coord_to board &&
    notAdjacent coord_from coord_to
  with 
  | NoPiece -> raise NoPiece
  | FullSquare -> raise FullSquare
  | Backwards -> raise Backwards
  | NotAdjacent -> raise NotAdjacent

(** [update piece coord_to] is [piece] with its coordinate changed to 
    [coord_to], and if [piece] has reached the opposite side of the board, 
    [piece] becomes a king. *)
let update piece coord_to : (coordinate * piece) =
  if piece.color = "red" && (snd coord_to = "8") 
  || piece.color = "white" && (snd coord_to = "1")
  then ((coord_to), {color = piece.color; king = true})
  else ((coord_to), piece)

(** [toggle_move state] is the boolean representing the opposite of the current 
    turn. *)
let toggle_move (state : state) = not (fst state)

let move coord_from coord_to state : state =
  let rec move_piece coord_from coord_to state : board =
    try 
      let valid = valid_move coord_from coord_to state in
      if valid = false then failwith "Should Never Get Here" else 
        let board = get_board state in
        let turn = get_turn state in 
        match board with 
        | [] -> []
        | h::t -> 
          if (fst h) = coord_from then
            let piece = pos_to_piece coord_from board in 
            let new_piece = update piece coord_to in 
            new_piece :: t else
            h :: move_piece coord_from coord_to (turn, t)
    with 
    | NoPiece -> raise NoPiece
    | FullSquare -> raise FullSquare
    | Backwards -> raise Backwards
    | NotAdjacent -> raise NotAdjacent
  in (toggle_move state, move_piece coord_from coord_to state)

(** [adjPieceCap coord_from coord_to] is true if [coord_to] is two diagonal 
    spaces away from [coord_from], and there is a piece of the opposing
    player's at the coordinate between [coord_from] and [coord_to]. Raises 
    [NotAdjCapture] if [coord_to] is not two diagonal spaces away from 
    [coord_from]. Raises [NoPiecetoCapture] if the coordinate between 
    [coord_from] and [coord_to] has no piece associated with it. Raises 
    [OwnPieceCap] if the piece associated with the coordinate between 
    [coord_from] and [coord_to] is the color of the current turn.*)
let adjPieceCap coord_from coord_to state : bool =
  try  
    let from_int = int_of_string (snd coord_from) in 
    let to_int = int_of_string (snd coord_to) in 
    let char = String.get (fst coord_from) 0 in
    let inc = Char.chr (Char.code char + 2) in 
    let dec = Char.chr (Char.code char - 2) in 
    if not ((fst coord_to = Char.escaped inc || fst coord_to = Char.escaped dec) 
            && (fst coord_to <> "i" && fst coord_to <> "j" && fst coord_to <> "_" 
                && fst coord_to <> "`" ) && 
            (from_int + 2 = to_int || from_int - 2 = to_int)
            && (to_int <> -1 && to_int <> 0 && to_int <> 9 && to_int <> 10))
    then raise NotAdjCapture else 
      let char_to = String.get (fst coord_to) 0 in 
      let char_opp = Char.chr ((Char.code char + Char.code char_to) / 2) in 
      let int_opp = ((from_int + to_int) / 2) in
      let coord_op = (Char.escaped char_opp, string_of_int int_opp) in
      let piece = pos_to_piece coord_op (get_board state) in 
      if get_turn_color state <> piece.color then true else raise OwnPieceCap
  with 
  | NotAdjCapture -> raise NotAdjCapture
  | Not_found -> raise NoPieceToCapture
  | OwnPieceCap -> raise OwnPieceCap

let valid_capture coord_from coord_to state : bool =
  try
    let board = get_board state in 
    let turn_color = get_turn_color state in
    noPiece coord_from state &&
    fullSquare coord_to board &&
    backwards turn_color coord_from coord_to board &&
    adjPieceCap coord_from coord_to state
  with 
  | NoPiece -> raise NoPiece
  | FullSquare -> raise FullSquare
  | Backwards -> raise Backwards
  | NotAdjCapture -> raise NotAdjCapture
  | NoPieceToCapture -> raise NoPieceToCapture
  | OwnPieceCap -> raise OwnPieceCap

let capture coord_from coord_to state : state = 
  try 
    let _ = valid_capture coord_from coord_to state in
    let rec capture_piece coord_from coord_to state : board =
      let board = get_board state in
      let turn = get_turn state in 
      match board with 
      | [] -> []
      | h::t -> 
        if (fst h) = coord_from then
          let piece = pos_to_piece coord_from board in 
          let new_piece = update piece coord_to in 
          new_piece :: t 
        else h :: capture_piece coord_from coord_to (turn, t)
    in 
    let new_state = (fst state, capture_piece coord_from coord_to state) in
    let rec remove_captured coord_from coord_to state : board =
      let board = get_board state in
      let turn = get_turn state in 
      let from_int = int_of_string (snd coord_from) in 
      let to_int = int_of_string (snd coord_to) in 
      let char = String.get (fst coord_from) 0 in
      let char_to = String.get (fst coord_to) 0 in 
      let char_opp = Char.chr ((Char.code char + Char.code char_to) / 2) in 
      let int_opp = ((from_int + to_int) / 2) in
      let coord_op = (Char.escaped char_opp, string_of_int int_opp) in
      match board with 
      | [] -> []
      | h::t -> 
        if (fst h) = coord_op then t
        else h :: remove_captured coord_from coord_to (turn, t)
    in (toggle_move new_state, remove_captured coord_from coord_to new_state)
  with 
  | NoPiece -> raise NoPiece
  | FullSquare -> raise FullSquare
  | Backwards -> raise Backwards
  | NotAdjCapture -> raise NotAdjCapture
  | NoPieceToCapture -> raise NoPieceToCapture
  | OwnPieceCap -> raise OwnPieceCap

(** [get_surrounding coordinate int] is a coordinate list containing 4 
    coordinates representing theoretical int = 1 for regular move, int = 2 for 
    captures *)
let get_surrounding coordinate int : coordinate list =
  let coord_int = int_of_string (snd coordinate) in 
  let int1 = string_of_int (coord_int + int) in 
  let int2 = string_of_int (coord_int - int) in 
  let coord_char = String.get (fst coordinate) 0 in
  let alph1 = Char.escaped (Char.chr (Char.code coord_char + int)) in 
  let alph2 = Char.escaped (Char.chr (Char.code coord_char - int)) in
  (alph1, int1) :: (alph1, int2) :: (alph2, int1) :: (alph2, int2) :: []

(** [list_moves coordinate state] is the tuple of possible moves and captures
    for [coordinate] given the current [state] of the game *)
let list_moves coordinate state : coordinate list * coordinate list =
  let moves = get_surrounding coordinate 1 in 
  let captures = get_surrounding coordinate 2 in 
  let combinelist = moves @ captures in  
  let rec list_moves_help coordinate list state (moves, captures) =
    match list with
    | [] -> (moves, captures)
    | h::t ->
      try
        if valid_move coordinate h state
        then list_moves_help coordinate t state (h::moves, captures)
        else failwith "Should never get here 1"
      with _ ->
      try  
        if valid_capture coordinate h state
        then list_moves_help coordinate t state (moves, h::captures)
        else failwith "Should never get here 2"
      with _ -> list_moves_help coordinate t state (moves, captures)
  in list_moves_help coordinate combinelist state ([], [])

let cant_move state =
  let board = get_board state in 
  let rec cant_move_help board_list = 
    match board_list with 
    | [] -> true
    | (h,_)::t -> 
      if list_moves h state <> ([], []) then false
      else cant_move_help t
  in cant_move_help board

let can_capture coord state : bool =
  snd (list_moves coord state) <> []

let any_capture state : bool =
  let board = get_board state in
  let rec any_capture_help board = 
    match board with 
    | [] -> false 
    | h::t ->
      if can_capture (fst h) state then true
      else any_capture_help t
  in any_capture_help board

(** valid move must capture *)
let vmmc coord_from coord_to state =
  try
    if ((valid_move coord_from coord_to state) && (any_capture state = false))
    then move coord_from coord_to state
    else raise MustCap
  with 
  | NoPiece -> raise NoPiece
  | FullSquare -> raise FullSquare
  | Backwards -> raise Backwards
  | NotAdjacent -> raise NotAdjacent
  | MustCap -> raise MustCap
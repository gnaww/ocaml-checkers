(** The type of the checker pieces *)
type piece = {
  color: string;
  king: bool;
}

(** The type of the coordinates on the board *)
type coordinate = string * string

(** The abstract value representing the game board *)
type board = (coordinate * piece) list

(** The abstract value representing the state of the game, containing the 
    the turn (red = true, white = false) and the current game board. *)
type state = (bool * board)

exception NoPiece
exception FullSquare
exception Backwards
exception NotAdjacent
exception NoPieceToCapture
exception NotAdjCapture
exception OwnPieceCap
exception MustCap

(** [init_board] is the initial board before any pieces are moved. *)
val init_board : board

(** [init_state] is red's turn (true) and init_board. *)
val init_state : state

(** [pos_to_piece coordinate board] is the piece associated with coordinate
    [coordinate] on board [board]. Requires a piece assocaited with the 
    coordinate. Raises [Not_found] if there is no piece associated with 
    [coordinate]. *)
val pos_to_piece : coordinate -> board -> piece

(** [get_color coord board] is the color of the piece associated with coordinate
    [coord] on board [board]. Requires a piece assocaited with the coordinate. 
    Raises [Not_found] if there is no piece associated with [coord]. *)
val get_color : coordinate -> board -> string

(** [get_board state] is the board represented by the state. *)
val get_board : state -> board

(** [is_king coord board] is true if the piece associated with coordinate 
    [coord] on board [board] is a king, and false otherwise. Requires a piece 
    associated with the coordinate. Raises [Not_found] if there is no piece 
    associated with [coord]. *)
val is_king : coordinate -> board -> bool 

(** [get_turn_color state] is the string representing the color of the turn. *)
val get_turn_color : state -> string

(** [is_empty coord board] is true if there is no piece associated with 
    coordinate [coord] on board [board], and false otherwise. *)
val is_empty : coordinate -> board -> bool

(** [noPiece coord state] is true if there is a piece associated with 
    coordinate [coord] on the board in state [state] and the piece is the color
    of the current turn. Raises [NoPiece] otherwise. *)
val noPiece : coordinate -> state -> bool

(** [fullSquare coord board] is true if coordinate [coord] associated with 
    board [board] is empty. Raises [FullSquare} otherwise. *)
val fullSquare : coordinate -> board -> bool

(** [backwards color coord_from coord_to board] is true if the piece at
    coordinate [coord_from] is a king, or if [coord_from] is "in front of"
    [coord_to], ie if [coord_from] has a lower number than [coord_to] for white
    pieces and a higher number for red pieces. Requires: there is a piece at
    coordinate [coord]. Raises [Backwards] if the piece at [coord] is not a
    king and [coord_to] is behind [coord_from]. *)
val backwards : string -> coordinate -> coordinate -> board -> bool

(** [notAdjacent coord_from coord_to] is true if [coord_from] is a corner space
    to [coord_to]. Raises [notAdjacent] otherwise. *)
val notAdjacent : coordinate -> coordinate -> bool

(** [valid_move] is true if [color], [coord_from], [coord_to], and [board]
    constitute a valid move. A move is not valid if no piece exists at 
    [coord_from], if there is already a piece a [coord_to], if the piece is 
    not a king and is attempting to move backwards, or if [coord_to] is not on 
    the board nor diagonally adjacent to [coord_from]. *)
val valid_move : coordinate -> coordinate -> state -> bool

(** [vmmc coord_from coord_to state] (valid move must capture) executes
    [move coord_from coord_to state] returning a new state, iff
    [valid_move coord_from coord_to state] is true and the player does not have
    any pieces for which [valid_capture from to state] is true for some pair of 
    coordinates [from] and [to] in the current state, [state].
    Raises exceptions in accordance [valid_move coord_from coord_to state].
    Raises [MustCap] if [valid_move] is true, but there was an available capture
    satisfying [valid_capture] that was not performed. *)
val vmmc : coordinate -> coordinate -> state -> state

(** [move coord_from coord_to state] is a state where the piece associated with
    [coord_from] has been changed to [coord_to]. Raises [NoPiece] if there is 
    no piece at [coord_from], or if the piece at [coord_from] is not the color
    of the current turn. Raises [FullSquare] if there is a piece associated 
    with [coord_to]. Raises [Backwards] if the piece associated with
    [coord_from] is not a king and [coord_to] is closer to the starting area of
    the color than [coord_from]. Raises [NotAdjacent] if [coord_to] is not
    diagonally adjacent to [coord_from]. *)
val move: coordinate -> coordinate -> state -> state

(** [valid_capture coord_from coord_to state] is true if a capture from
    [coord_from] to [coord_to] is a valid checkers capture. Raises [NoPiece] 
    if there is no piece at [coord_from], or if the piece at [coord_from] is 
    not the color of the current turn. Raises [FullSquare] if there is a piece 
    associated with [coord_to]. Raises [Backwards] if the piece associated with
    [coord_from] is not a king and [coord_to] is closer to the starting area of
    the color than [coord_from]. Raises [NotAdjCapture] if [coord_to] is not
    two diagonal spaces away from [coord_from]. Raises [NoPiecetoCapture] if
    the coordinate between [coord_from] and [coord_to] has no piece associated
    with it. Raises [OwnPieceCap] if the piece associated with the coordinate
    between [coord_from] and [coord_to] is the color of the current turn. *)
val valid_capture : coordinate -> coordinate -> state -> bool

(** [capture coord_from coord_to] is a state where the piece associated with
    [coord_from] has been changed to [coord_to] and the piece associated with
    the coordinate between [coord_from] and [coord_to] has been removed. Raises
    [NoPiece] if there is no piece at [coord_from], or if the piece at 
    [coord_from] is not the color of the current turn. Raises [FullSquare] if 
    there is a piece associated with [coord_to]. Raises [Backwards] if the piece
    associated with [coord_from] is not a king and [coord_to] is closer to the 
    starting area of the color than [coord_from]. Raises [NotAdjCapture] if 
    [coord_to] is not two diagonal spaces away from [coord_from]. Raises 
    [NoPiecetoCapture] if the coordinate between [coord_from] and [coord_to] 
    has no piece associated with it. Raises [OwnPieceCap] if the piece 
    associated with the coordinate between [coord_from] and [coord_to] is the 
    color of the current turn.*)
val capture : coordinate -> coordinate -> state -> state

(** [cant_move state] is true if the player with the current turn is unable
    to make any legal moves. False otherwise. *)
val cant_move : state -> bool

(** [can_capture coord state] is true if the piece at coordinate [coord] can 
    capture any piece that satisfies [valid_capture coord coord_to state] for 
    some coordinate [coord_to]. False otherwise. *)
val can_capture :  coordinate -> state -> bool

(** [any_capture state] is true if any piece on the board in state [state] can 
    make a capture satisfying [valid_capture coord_from coord_to state] for some 
    pair of coordinates [coord_from] and [coord_to]. False otherwise. *)
val any_capture : state -> bool


(** [list_moves coordinate state] is the tuple of possible moves and captures
    for [coordinate] given the current [state] of the game *)
val list_moves : coordinate -> state -> coordinate list * coordinate list
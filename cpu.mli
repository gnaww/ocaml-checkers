(** [easyAI state] takes in a state of a game board where it is white's turn,
    and returns a state where white has moved. It takes the first capture
    available, otherwise it moves the first piece in the state. *)
val easyAI : State.state -> State.state

(** [mediumAI state] takes in a state of a game board where it is white's turn,
    and returns a state where white has moved. It takes a random capture
    available, and if there are no captures available, it moves a random 
    piece. *)
val mediumAI : State.state -> State.state

(** [hardAI state] takes in a state of a game board where it is white's turn, 
    and returns a state where white has moved. It takes the best move available
    by looking depth 2 into the game tree. *)
val hardAI : State.state -> State.state

(** [white_pieces board pieces] is the list of white pieces on board [board]. *)
val white_pieces : State.board -> (State.coordinate * State.piece) list -> 
  (State.coordinate * State.piece) list

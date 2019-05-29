type tree

(** [best_move state] takes in a State.state [state] and returns the best move
    available to the current turn's player based on the heuristic evaluation 
    of the board
*)
val best_move : State.state -> (bool * State.coordinate) * State.state

(** [hint state] takes in a State.state [state] and returns the coordinate pair
    of the player's next best move, determined by the game tree. 
*)
val hint: State.state -> State.coordinate * State.coordinate
open State
open Command
open Cpu

(** [execute_move coord1 coord2 state] applies [State.move] 
    to move the piece from [coord1] to [coord2] and prints corresponding 
    messages on invalid moves
*) 
let execute_move coord1 coord2 state = 
  try State.vmmc coord1 coord2 state
  with 
  | State.NoPiece -> ANSITerminal.(print_string [red] ("Invalid piece. " ^ 
                                                       "Did you choose the wrong " ^ 
                                                       "coordinate? Try again. \n")); 
    state
  | State.FullSquare -> ANSITerminal.(print_string [red] ("Invalid move. " ^ 
                                                          "There is a piece blocking "
                                                          ^"your way. Try again. \n")); 
    state
  | State.Backwards -> ANSITerminal.(print_string [red] ("Invalid move. You cannot"^
                                                         " move backwards "^ 
                                                         "unless you are a King piece"
                                                         ^". Try again. \n"));
    state
  | State.NotAdjacent -> ANSITerminal.(print_string [red] ("Invalid move. You "^ 
                                                           "must move one square"^
                                                           " diagonally forward "^
                                                           "or capture a piece."
                                                           ^" Try again. \n"));
    state 
  | State.MustCap -> ANSITerminal.(print_string [red] ("Invalid move. You "^ 
                                                       "must make any available "^
                                                       "captures. Try again. \n"));
    state  

(** [execute_capture coord1 coord2 state] applies [State.capture] 
    to move the piece from [coord1] to [coord2] and prints corresponding 
    messages on invalid captures
*)
let execute_capture coord1 coord2 state = 
  try
    let capstate = State.capture coord1 coord2 state in  
    if State.can_capture coord2 (fst state, snd capstate) then (fst state, snd 
                                                                  capstate)
    else capstate 
  with 
  | State.NoPiece -> ANSITerminal.(print_string [red] ("Invalid piece. " ^ 
                                                       "Did you choose the wrong " ^ 
                                                       "coordinate? Try again. \n")); 
    state
  | State.FullSquare -> ANSITerminal.(print_string [red] ("Invalid move. " ^ 
                                                          "There is a piece blocking "
                                                          ^"your way. Try again. \n")); 
    state
  | State.Backwards -> ANSITerminal.(print_string [red] ("Invalid move. You "^
                                                         "cannot move backwards "^ 
                                                         "unless you are a King "^
                                                         "piece. Try again. \n"));
    state
  | State.NotAdjCapture -> ANSITerminal.(print_string [red] ("Invalid move. You "^ 
                                                             "cannot capture a "^
                                                             "piece that isn't "^
                                                             "diagonally adjacent"
                                                             ^". Try again. \n"));
    state 
  | State.NoPieceToCapture -> ANSITerminal.(print_string [red] ("Invalid move. There"^ 
                                                                " is no available "^
                                                                "piece to capture"
                                                                ^". Try again. \n"));
    state
  | State.OwnPieceCap -> ANSITerminal.(print_string [red] ("Invalid move. You "^ 
                                                           "cannot capture your "^
                                                           "own piece. Try again. \n"));
    state
  | _ -> print_endline "Uncaught exception! This shouldn't have happened :("; 
    state  


(** [find_piece col row board] returns the color and whether or not the piece is
    a king for the piece at [row, col] on the board. Returns ("NoPiece", false)
    if there is no piece at [row, col]
*)
let rec find_piece col row board =
  match board with
  | [] -> ("NoPiece",false)
  | h::t -> let coord = fst h in
    if ((String.equal (fst coord) col) && (String.equal (snd coord) row)) then
      ((snd h).color, (snd h).king)
    else find_piece col row t

(** [convert_y row col] is the y-axis label for the [row] if [col] = 1, else
    it is the empty string *)
let convert_y row col =
  if col = 1 then string_of_int row else ""

(** [convert_col col] is a string containing the conversion of column to
    a letter between a-h *)
let convert_col col = 
  String.make 1 (Char.chr (col + 96))

(** [print_board board] prints out the board representation of the list of
    pieces [board]
*)
let print_board state hint = 
  let board = snd state in 
  ANSITerminal.(print_string [] "\n     a     b     c     d     e     f     g     h\n");
  ANSITerminal.(print_string [] "  #################################################\n");
  ANSITerminal.(print_string [] "  #     #     #     #     #     #     #     #     #\n");
  for row = 1 to 8 do
    let new_row = 9 - row in 
    if new_row <> 8 then 
      ANSITerminal.(print_string [] ("  #     #     #     #     #     #     #     #     #\n"
                                     ^"  #################################################\n"
                                     ^"  #     #     #     #     #     #     #     #     #\n")); 
    for col = 1 to 8 do
      let y_coord = convert_y new_row col in
      let str_nopiece = y_coord ^ " #    " in
      let str_red =  y_coord ^ " # 🔴 " in
      let str_white = y_coord ^ " # ⚪ " in
      let king_red = y_coord ^ " # 🅾️  " in
      let king_white = y_coord ^ " # 0️⃣  " in
      let piece = find_piece (convert_col col) (string_of_int new_row) board in
      if fst piece = "NoPiece" then 
        ANSITerminal.(print_string [] str_nopiece)
      else if (snd piece) = true && String.equal (fst piece) "red" then
        ANSITerminal.(print_string [] king_red) 
      else if (snd piece) = true && String.equal (fst piece) "white" then
        ANSITerminal.(print_string [] king_white) 
      else if String.equal (fst piece) "red" then
        ANSITerminal.(print_string [] str_red)
      else 
        ANSITerminal.(print_string [] str_white);

      if col = 8 then
        ANSITerminal.(print_string [] " #\n");
      if col = 8 && row = 8 then
        ANSITerminal.(print_string [] ("  #     #     #     #     #     #     #     #     #"
                                       ^ "\n  #################################################\n"));
    done
  done



(** [execute_hint state] applies [Tree.hint] and highlights valid
    moves on the board for the piece at [coord]
*)
let execute_hint state = 
  let board = snd state in 
  ANSITerminal.(print_string [] "\n     a     b     c     d     e     f     g     h\n");
  ANSITerminal.(print_string [] "  #################################################\n");
  ANSITerminal.(print_string [] "  #     #     #     #     #     #     #     #     #\n");
  for row = 1 to 8 do
    let new_row = 9 - row in 
    if new_row <> 8 then 
      ANSITerminal.(print_string [] ("  #     #     #     #     #     #     #     #     #\n"
                                     ^"  #################################################\n"
                                     ^"  #     #     #     #     #     #     #     #     #\n")); 
    for col = 1 to 8 do
      let y_coord = convert_y new_row col in
      let pre_str = y_coord ^ " # " in 
      let pre_str_piece = y_coord ^ " #" in
      let str_nopiece = "   " in 
      let str_red = " 🔴 " in
      let str_white = " ⚪ " in
      let king_red = " 🅾️  " in
      let king_white = " 0️⃣  " in
      let highlight = ANSITerminal.on_yellow in
      let piece = find_piece (convert_col col) (string_of_int new_row) board in
      let (coord_from, coord_to) = Tree.hint state in 
      if fst piece = "NoPiece" &&
         (convert_col col) = fst coord_to 
         && (string_of_int new_row) = snd coord_to then (
        ANSITerminal.(print_string [] pre_str); 
        ANSITerminal.(print_string [highlight] str_nopiece) )
      else if fst piece = "NoPiece" then (
        ANSITerminal.(print_string [] pre_str); 
        ANSITerminal.(print_string [] str_nopiece) )
      else if (snd piece) = true && String.equal (fst piece) "red" 
              && (convert_col col) = fst coord_from 
              && (string_of_int new_row) = snd coord_from then (
        ANSITerminal.(print_string [] pre_str_piece); 
        ANSITerminal.(print_string [highlight] king_red) )
      else if (snd piece) = true && String.equal (fst piece) "red" then (
        ANSITerminal.(print_string [] pre_str_piece); 
        ANSITerminal.(print_string [] king_red) )
      else if (snd piece) = true && String.equal (fst piece) "white" 
              && (convert_col col) = fst coord_from 
              && (string_of_int new_row) = snd coord_from then (
        ANSITerminal.(print_string [] pre_str_piece); 
        ANSITerminal.(print_string [highlight] king_white) )
      else if (snd piece) = true && String.equal (fst piece) "white" then (
        ANSITerminal.(print_string [] pre_str_piece); 
        ANSITerminal.(print_string [] king_white) )
      else if String.equal (fst piece) "red" 
           && (convert_col col) = fst coord_from 
           && (string_of_int new_row) = snd coord_from then (
        ANSITerminal.(print_string [] pre_str_piece); 
        ANSITerminal.(print_string [highlight] str_red) )
      else if String.equal (fst piece) "red" then (
        ANSITerminal.(print_string [] pre_str_piece); 
        ANSITerminal.(print_string [] str_red) )
      else if String.equal (fst piece) "white" 
           && (convert_col col) = fst coord_from 
           && (string_of_int new_row) = snd coord_from then (
        ANSITerminal.(print_string [] pre_str_piece) ;
        ANSITerminal.(print_string [highlight] str_white))
      else if String.equal (fst piece) "white" then (
        ANSITerminal.(print_string [] pre_str_piece) ;
        ANSITerminal.(print_string [] str_white) )
      else ();

      if col = 8 then
        ANSITerminal.(print_string [] " #\n");
      if col = 8 && row = 8 then
        ANSITerminal.(print_string [] ("  #     #     #     #     #     #     #     #     #"
                                       ^ "\n  #################################################\n"));
    done
  done


(** [calculate_pieces board red white] returns the number of remaining red and
    white pieces left on the board
*)
let rec calculate_pieces board red white =
  match board with 
  | [] -> (red, white)
  | h::t -> if String.equal (snd h).color "white" then calculate_pieces t red 
        (white+1)
    else calculate_pieces t (red+1) white

(** [calculate_scores state] is the scores of each player, determined by
    the number of pieces each player has captured
*)
let calculate_scores state = 
  let (r,w) = calculate_pieces (snd state) 0 0 in (12 - w, 12 - r)

(** [print_scores state] prints out the scores of each player from
    [calculate_scores state]
*)
let print_scores state =
  let scores = calculate_scores state in 
  ANSITerminal.(print_string [green] ("Player Red Score: " ^ string_of_int 
                                        (fst scores) ^ "\n" 
                                      ^ "Player White Score: " ^ string_of_int 
                                        (snd scores) ^ "\n"))

(** [is_win state] returns whether or not the game has ended because a player
    has either captured all 12 opposing pieces or there is no valid move for a
    player
*)
let is_win state = 
  let score = calculate_scores state in
  (fst score = 12) || (snd score = 12) || (State.cant_move state)

(** [print_help ()] prints out the game instructions to the terminal *)
let print_help _ =
  ANSITerminal.(print_string [blue; Bold] ("To play the game you have several "
                                           ^"commands: \n"));
  ANSITerminal.(print_string [blue; Bold] ("move b8 to c7"));
  ANSITerminal.(print_string [blue] (": Moves the piece at coordinate"^
                                     " b8 to the coordinate c7.\n"));
  ANSITerminal.(print_string [blue; Bold] ("capture b8 to d6"));
  ANSITerminal.(print_string [blue] (": Moves the piece at coordinate"^
                                     " b8 to the coordinate d6, capturing the"^
                                     " piece in between.\n"));
  ANSITerminal.(print_string [blue; Bold] ("hint"));
  ANSITerminal.(print_string [blue] (": Highlights the best possible"^
                                     " move\n"));
  ANSITerminal.(print_string [blue; Bold] ("board"));
  ANSITerminal.(print_string [blue] (": Prints out the board.\n"));
  ANSITerminal.(print_string [blue; Bold] ("score"));
  ANSITerminal.(print_string [blue] (": Displays the number of captured"^
                                     " pieces for each player.\n"));
  ANSITerminal.(print_string [blue; Bold] ("quit"));
  ANSITerminal.(print_string [blue] (": Quits the game.\n"));
  ANSITerminal.(print_string [blue] "Type ");
  ANSITerminal.(print_string [blue; Bold] "help");
  ANSITerminal.(print_string [blue] " to see these instructions again!\n")

(** [parse_command state bool aiLevel hint] parses the command the player enters into the 
    terminal and executes the command and loops until the game ends or a user 
    quits. [bool] is true for a one-player game and false for a two-player game.
    [aiLevel] is 1 for easy, 2 for medium, and 3 for hard. [hint] is a bool 
    indicating if the hint command was entered and the board with the highlighted
    hint should be printed.
*)
let rec parse_command (state : State.state) (bool : bool) (aiLevel : int) (hint:bool)=

  if (not hint) 
  then print_board state false
  else execute_hint state ; 
  if(is_win state) then (
    if(fst state) then
      ANSITerminal.(print_string [Bold] ("Player White wins. Congratulations!\n"); 
                    exit 0)
    else  ANSITerminal.(print_string [Bold] ("Player Red wins. Congratulations!\n"); 
                        exit 0)
  )
  else 
  if bool && not (fst state) && aiLevel = 1 then
    let new_state = easyAI state in 
    parse_command new_state bool aiLevel false
  else if bool && not (fst state) && aiLevel = 2 then
    let new_state = mediumAI state in 
    parse_command new_state bool aiLevel false
  else if bool && not (fst state) && aiLevel = 3 then
    let new_state = hardAI state in 
    parse_command new_state bool aiLevel false
  else if (fst state) then
    ANSITerminal.(print_string [Bold] ("Player Red's move:\n"))
  else
    ANSITerminal.(print_string [Bold] ("Player White's move:\n")); 
  print_string  "> ";
  match read_line () with 
  | exception End_of_file -> () 
  | str -> 
    try 
      let command = Command.parse str in 
      match command with 
      | Move (coord1, coord2) -> 
        let new_state = execute_move coord1 coord2 state in
        parse_command new_state bool aiLevel false
      | Capture (coord1, coord2) -> 
        let new_state = execute_capture coord1 coord2 state in
        if (fst state) = (fst new_state) && state <> new_state
        then parse_command_cap new_state coord2 bool aiLevel false
        else parse_command new_state bool aiLevel false
      | Hint -> parse_command state bool aiLevel true
      | Board -> parse_command state bool aiLevel false
      | Quit -> (ANSITerminal.(print_string [magenta; Bold] ("Thank you for playing "
                                                             ^"OCaml Checkers!\n")); 
                 exit 0)
      | Help -> print_help ();
        parse_command state bool aiLevel false
      | Score -> let _ = print_scores state in 
        parse_command state bool aiLevel false
    with _ -> ANSITerminal.(print_string [red] ("Invalid or illegal command.\n"));
      parse_command state bool aiLevel false 

(** [parse_command_cap state coord bool aiLevel hint] parses the command the 
    player enters into the terminal and executes the command, given that there 
    is an opportunity to capture on the board. It loops until the game ends or 
    a user quits. [bool] is true for a one-player game and false for a two-player 
    game. [aiLevel] is 1 for easy, 2 for medium, and 3 for hard. [hint] is a bool 
    indicating if the hint command was entered and the board with the highlighted
    hint should be printed. *)
and parse_command_cap (state : State.state) (coord : State.coordinate) bool aiLevel hint =
  if (not hint) 
  then print_board state false
  else execute_hint state ; 
  if(is_win state) then (
    if(fst state) then
      ANSITerminal.(print_string [Bold] ("Player White wins. Congratulations!\n"); 
                    exit 0)
    else  ANSITerminal.(print_string [Bold] ("Player Red wins. Congratulations!\n"); 
                        exit 0)
  )
  else if bool && not (fst state) && aiLevel = 1 then
    let new_state = easyAI state in 
    parse_command_cap new_state coord bool aiLevel false
  else if bool && not (fst state) && aiLevel = 2 then
    let new_state = mediumAI state in 
    parse_command_cap new_state coord bool aiLevel false
  else if bool && not (fst state) && aiLevel = 3 then
    let new_state = hardAI state in 
    parse_command_cap new_state coord bool aiLevel false
  else 
  if (fst state) then
    ANSITerminal.(print_string [Bold] ("Player Red's move:\n "))
  else
    ANSITerminal.(print_string [Bold] ("Player White's move:\n "));
  print_string  "> ";
  match read_line () with 
  | exception End_of_file -> () 
  | str -> 
    try 
      let command = Command.parse str in 
      match command with 
      | Move (_, _) -> ANSITerminal.(print_string [red] ("Invalid move. " ^ 
                                                         "You must make a capture"
                                                         ^" with the piece you just "
                                                         ^"moved. Try again. \n"));
        parse_command_cap state coord bool aiLevel false
      | Capture (coord1, coord2) ->
        if coord1 = coord then 
          let new_state = execute_capture coord1 coord2 state in 
          (parse_command new_state bool aiLevel false)
        else ANSITerminal.(print_string [red] ("Invalid move. You must make a "
                                               ^"capture with the piece you just "
                                               ^"moved. Try again. \n"));
        (parse_command_cap state coord bool aiLevel false)
      | Hint -> parse_command_cap state coord bool aiLevel true
      | Board -> parse_command_cap state coord bool aiLevel false
      | Quit -> (ANSITerminal.(print_string [magenta; Bold] ("Thank you for "
                                                             ^"playing OCaml "
                                                             ^"Checkers!\n")); 
                 exit 0)
      | Help -> print_help ();
        parse_command_cap state coord bool aiLevel false
      | Score -> let _ = print_scores state in
        parse_command_cap state coord bool aiLevel false
    with _ -> ANSITerminal.(print_string [red] ("Invalid or illegal command.\n"));
      parse_command_cap state coord bool aiLevel false 

(** [play_game ()] starts the game of checkers with an initial board state of
    the pieces placed in their starting positions. *)
let play_game () = 
  parse_command State.init_state false 0 false

(** [play_one_player_easy ()] starts the game of checkers with an initial board
    state of the pieces placed in their starting positions and the easy AI *)
let play_one_player_easy _ = parse_command State.init_state true 1 false

(** [play_one_player_medium ()] starts the game of checkers with an initial 
    board state of the pieces placed in their starting positions and the medium 
    AI *) 
let play_one_player_medium _ = parse_command State.init_state true 2 false

(** [play_one_player_hard ()] starts the game of checkers with an initial 
    board state of the pieces placed in their starting positions and the hard 
    AI *)
let play_one_player_hard _ = parse_command State.init_state true 3 false

(** [main ()] prompts for the game to play, asks whether you would like to play
    with 2 players or 1, asks the difficulty of the AI, then starts the game. *)
let main () =
  ANSITerminal.(print_string [blue; Bold; Underlined] ("\n\nWelcome to OCaml "
                                                       ^"Checkers!\n\n"));
  print_help ();
  ANSITerminal.(print_string [blue; Bold] ("\n1 Player or 2 Players? (Type '1' or '2') \n"));
  print_string  "> ";
  match read_line () with 
  | exception End_of_file -> () 
  | str -> if "1" = str then (
      ANSITerminal.(print_string [blue; Bold] ("\nEasy, Medium, or Hard?\n"));
      print_string  "> ";
      match read_line () with
      | exception End_of_file -> ()
      | str -> if "easy" = (String.lowercase_ascii str) then play_one_player_easy () 
        else if "medium" = (String.lowercase_ascii str) then play_one_player_medium ()
        else if "hard" = (String.lowercase_ascii str) then play_one_player_hard ()
        else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n")); 
        exit 0
    )
    else if "2" = str then play_game ()
    else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n")); exit 0

(* Execute the game engine. *)
let () = main ()
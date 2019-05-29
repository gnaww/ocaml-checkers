open State
open Tree

let rec white_pieces (board : State.board) pieces = 
  match board with 
  | [] -> pieces
  | h::t -> if (snd h).color = "white" then white_pieces t (h::pieces) else 
      white_pieces t pieces

(** [easyAIhelp coord state unused_pieces] is the state where the piece at 
    [coord] has moved in state [state]. If [coord] has possible captures, it
    takes the first possible capture. Otherwise, if [coord] has possible moves, 
    it makes the first possible move. If [coord] has neither available captures
    nor available moves, [easyAIhelp] tries the same algorithm with the next 
    piece in [unused_pieces]. If no moves or captures are available in 
    [unused_pieces], [easyAIhelp] is [state]. [easyAIhelp] allows for double
    captures. *)
let rec easyAIhelp coord state unused_pieces =
  try
    let (moves, captures) = State.list_moves coord state in 
    if captures <> [] then (
      let new_coord = List.hd captures in 
      let new_state = State.capture coord new_coord state in 
      if State.can_capture new_coord (fst state, snd new_state) 
      then easyAIhelp new_coord (fst state, snd new_state) [] else new_state)
    else if moves <> [] then State.vmmc coord (List.hd moves) state
    else 
      match unused_pieces with 
      | [] -> state 
      | h::t -> easyAIhelp (fst h) state t
  with
  | MustCap ->
    match unused_pieces with 
    | [] -> state 
    | h::t -> easyAIhelp (fst h) state t

let easyAI (state : state) : state  = 
  let white_pieces = white_pieces (snd state) [] in
  match white_pieces with 
  | [] -> failwith "Should not get here"
  | h::t -> easyAIhelp (fst h) state t

(** [rand_piece pieces] is a tuple with the first of the tuple as a random
    piece in [pieces] and the second of the tuple as a list of al the other
    pieces in the list [pieces]. *)
let rec rand_piece pieces  =
  let () = Random.self_init () in 
  let rand_num =  Random.int (List.length pieces) in 
  let chosen_piece = List.nth pieces rand_num in 
  let unused_pieces = List.filter (fun x -> x <> chosen_piece) pieces in 
  (chosen_piece, unused_pieces)

(** [mediumAI_helper chosen unused state] is the state where piece [chosen] has
    been moved in state [state]. If [chosen] has possible captures, it takes a 
    random capture. If [chosen] has possible moves, it takes a random move. If
    [chosen] has no possible moves or captures, or if a [MustCap] error is
    thrown, [mediumAI_helper] tries the same algorithm with a random piece in
    the list [unused]. If no moves or captures are available in [unused],
    [mediumAI_helper] is [state]. [mediumAI_helper] allows for double captures.
*)
let rec mediumAI_helper chosen unused state = 
  try
    let (moves, captures) = State.list_moves chosen state in 
    if captures <> [] then 
      let () = Random.self_init () in 
      let rand_num  =  Random.int (List.length captures) in 
      let chosen_cap = List.nth captures rand_num in 
      let new_state = State.capture chosen chosen_cap state in  
      if State.can_capture chosen_cap (fst state, snd new_state)
      then mediumAI_helper chosen_cap [] (fst state, snd new_state)
      else new_state 
    else if moves <> [] then 
      let () = Random.self_init () in 
      let rand_num  =  Random.int (List.length moves) in 
      let chosen_move = List.nth moves rand_num in 
      State.vmmc chosen chosen_move state 
    else mediumAI state 
  with 
  | MustCap -> mediumAI state 

and mediumAI (state:state) : state = 
  let pieces = white_pieces (snd state) [] in 
  if pieces = [] then failwith "Should not get here"
  else 
    let (chosen, unused) = rand_piece pieces in 
    mediumAI_helper (fst chosen) unused state

let rec hardAI (state:state) : state =
  let best_move = best_move state in 
  let new_state =snd best_move in
  if (fst (fst best_move)) = false && State.can_capture (snd (fst best_move)) (fst state, snd new_state)
  then hardAI (fst state, snd new_state)
  else new_state




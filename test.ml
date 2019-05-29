open State
open Command
open Cpu
open OUnit2

let pp_string s = "\"" ^ s ^ "\""

let interpret coord_from coord_to (state : State.state) =
  try 
    let result = valid_move coord_from coord_to state in 
    match result with 
    | true -> "true"
    | false -> "false"
  with 
  | NoPiece -> "No Piece"
  | FullSquare -> "Full Square"
  | Backwards -> "Backwards"
  | NotAdjacent -> "Not Adjacent"

let make_valid_move_test 
    (name : string)
    (coord_from : State.coordinate)
    (coord_to : State.coordinate)
    (state : State.state)
    (expected_output : string) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (interpret coord_from coord_to state) 
        ~printer:(pp_string))

let empty_board = (true, [])
let two_piece_board : State.state = 
  (true, [(("f", "3"), {color = "red"; king = false}); 
          (("g", "4"), {color = "white"; king = false})])
let one_king_board : State.state = (true, [(("f", "3"), {color = "red"; 
                                                         king = true})])

let valid_move_tests =
  [
    make_valid_move_test "No Piece" ("e", "2") ("d", "3") two_piece_board 
      "No Piece";
    make_valid_move_test "Full Square" ("f", "3") ("g", "4") two_piece_board 
      "Full Square";
    make_valid_move_test "Backwards" ("f", "3") ("e", "2") two_piece_board 
      "Backwards";
    make_valid_move_test "King Backwards" ("f", "3") ("e", "2") one_king_board 
      "true";
    make_valid_move_test "Not Adjacent Num" ("f", "3") ("f", "5") 
      two_piece_board "Not Adjacent";
    make_valid_move_test "Not Adjacent Letter"("f", "3") ("g", "3") 
      two_piece_board "Not Adjacent";
    make_valid_move_test "Move Wrong Color" ("g", "4") ("h", "3") 
      two_piece_board "No Piece";
    make_valid_move_test "Valid Adjacent" ("f", "3") ("e", "4") two_piece_board 
      "true"
  ]

let make_move_test 
    (name : string)
    (coord_from : State.coordinate)
    (coord_to : State.coordinate)
    (state : State.state)
    (expected_output : State.state) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (move coord_from coord_to state))

let two_piece_board_2 : State.state = 
  (false, [(("e", "4"), {color = "red"; king = false}); 
           (("g", "4"), {color = "white"; king = false})])
let one_king_board_2 : State.state = (false, [(("e", "4"), {color = "red"; 
                                                            king = true})])
let one_king_board_3 : State.state = (false, [(("e", "2"), {color = "red"; 
                                                            king = true})])
let full_board_1 : State.state = 
  (false, [(("b", "1"), {color = "red"; king = false});
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
           (("g", "4"), {color = "red"; king = false});
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
          ])
let full_board_2 : State.state = 
  (true, [(("b", "1"), {color = "red"; king = false});
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
          (("g", "4"), {color = "red"; king = false});
          (("b", "5"), {color = "white"; king = false});
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
         ])

let make_king_board : State.state = 
  (true, [(("e", "7"), {color = "red"; king = false}); 
          (("g", "4"), {color = "white"; king = false})])

let make_king_board_2 : State.state = 
  (false, [(("d", "8"), {color = "red"; king = true}); 
           (("g", "4"), {color = "white"; king = false})])

let move_tests =
  [
    make_move_test "Two piece standard" ("f", "3") ("e", "4") two_piece_board 
      two_piece_board_2;
    make_move_test "one king standard" ("f", "3") ("e", "4") one_king_board 
      one_king_board_2;
    make_move_test "one king backwards" ("f", "3") ("e", "2") one_king_board 
      one_king_board_3;
    make_move_test "full board" ("h", "3") ("g", "4") State.init_state 
      full_board_1;
    make_move_test "full board 2" ("a", "6") ("b", "5") full_board_1 
      full_board_2;
    make_move_test "make king" ("e", "7") ("d", "8") make_king_board 
      make_king_board_2
  ]


let interpretCap coord_from coord_to (state : State.state) =
  try 
    let result = valid_capture coord_from coord_to state in 
    match result with 
    | true -> "true"
    | false -> "false"
  with 
  | NoPiece -> "No Piece"
  | FullSquare -> "Full Square"
  | Backwards -> "Backwards"
  | NotAdjCapture -> "Not Adjacent Capture"
  | NoPieceToCapture -> "No Piece to Capture"
  | OwnPieceCap -> "Own Piece Capture"

let empty_board = (true, [])
let board1 : State.state = 
  (true, [(("f", "3"), {color = "red"; king = false}); 
          (("g", "4"), {color = "white"; king = false});
          (("e", "2"), {color = "white"; king = false});
          (("g", "3"), {color = "white"; king = false})])
let board2 : State.state = 
  (true, [(("f", "3"), {color = "red"; king = false}); 
          (("h", "5"), {color = "red"; king = false})])
let board3 : State.state = 
  (false, [(("f", "3"), {color = "red"; king = false}); 
           (("g", "4"), {color = "white"; king = true});
           (("e", "4"), {color = "white"; king = false})])
let board4 : State.state = 
  (true, [(("f", "3"), {color = "red"; king = false}); 
          (("g", "4"), {color = "red"; king = false})])

let white_cap_board : State.state =
  (false, [(("f", "6"), {color = "red"; king = false}); 
           (("e", "7"), {color = "white"; king = false});
           (("a", "8"), {color = "red"; king = false})])

let make_valid_capture_test     
    (name : string)
    (coord_from : State.coordinate)
    (coord_to : State.coordinate)
    (state : State.state)
    (expected_output : string) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (interpretCap coord_from coord_to state) 
        ~printer:(pp_string))

let valid_capture_tests =
  [
    make_valid_capture_test "No Piece" ("e", "2") ("d", "3") board1 "No Piece";
    make_valid_capture_test "Full Square" ("f", "3") ("h", "5") board2 
      "Full Square";
    make_valid_capture_test "Backwards" ("f", "3") ("d", "1") board1 "Backwards";
    make_valid_capture_test "Not Adjacent Num" ("f", "3") ("f", "5") board1 
      "Not Adjacent Capture";
    make_valid_capture_test "Not Adjacent Letter" ("f", "3") ("h", "3") board1 
      "Not Adjacent Capture";
    make_valid_capture_test "No Piece to Cap" ("f", "3") ("d", "5") board2 
      "No Piece to Capture";
    make_valid_capture_test "Own Piece Cap" ("f", "3") ("h", "5") board4 
      "Own Piece Capture";
    make_valid_capture_test "Valid Capture" ("f", "3") ("h", "5") board1 "true";
    make_valid_capture_test "Valid King Backwards Capture" ("g", "4") ("e", "2") 
      board3 "true";
    make_valid_capture_test "Regular Move" ("f", "3") ("e", "4") board2 
      "Not Adjacent Capture";
    make_valid_capture_test "White Capture" ("e", "4") ("g", "2") board3 "true";
    make_valid_capture_test "white2" ("e", "7") ("g", "5") white_cap_board "true"
  ]

let make_capture_test
    (name : string)
    (coord_from : State.coordinate)
    (coord_to : State.coordinate)
    (state : State.state)
    (expected_output : State.state) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (capture coord_from coord_to state))

let three_piece_board : State.state = 
  (true, [(("f", "3"), {color = "red"; king = false}); 
          (("g", "4"), {color = "white"; king = false});
          (("a", "8"), {color = "white"; king = false})])

let three_piece_board_2 : State.state = 
  (false, [(("h", "5"), {color = "red"; king = false});
           (("a", "8"), {color = "white"; king = false})])

let king_capture_board : State.state =
  (true, [(("f", "3"), {color = "red"; king = true}); 
          (("g", "4"), {color = "white"; king = true});
          (("e", "2"), {color = "white"; king = true})])

let king_capture_board_2 : State.state =
  (false, [(("d", "1"), {color = "red"; king = true}); 
           (("g", "4"), {color = "white"; king = true})])

let make_king_cap_board : State.state = 
  (true, [(("f", "6"), {color = "red"; king = false}); 
          (("e", "7"), {color = "white"; king = false});
          (("a", "8"), {color = "white"; king = false})])

let make_king_cap_board_2 : State.state = 
  (false, [(("d", "8"), {color = "red"; king = true}); 
           (("a", "8"), {color = "white"; king = false})])

let white_cap_board : State.state =
  (false, [(("f", "6"), {color = "red"; king = false}); 
           (("e", "7"), {color = "white"; king = false});
           (("a", "8"), {color = "red"; king = false})])

let white_cap_board_2 : State.state =
  (true, [(("g", "5"), {color = "white"; king = false});
          (("a", "8"), {color = "red"; king = false})])

let white_cap_board_3 : State.state =
  (false, [(("f", "6"), {color = "red"; king = false}); 
           (("e", "7"), {color = "white"; king = false});
           (("a", "8"), {color = "red"; king = false});
           (("d", "6"), {color = "red"; king = false})])

let white_cap_board_4 : State.state =
  (true, [(("f", "6"), {color = "red"; king = false}); 
          (("c", "5"), {color = "white"; king = false});
          (("a", "8"), {color = "red"; king = false})])

let capture_tests =
  [
    make_capture_test "standard" ("f", "3") ("h", "5") three_piece_board 
      three_piece_board_2;
    make_capture_test "kings" ("f", "3") ("d", "1") king_capture_board 
      king_capture_board_2;
    make_capture_test "make king" ("f", "6") ("d", "8") make_king_cap_board 
      make_king_cap_board_2;
    make_capture_test "white cap1" ("e", "7") ("g", "5") white_cap_board 
      white_cap_board_2;
    make_capture_test "white cap2" ("e", "7") ("c", "5") white_cap_board_3 
      white_cap_board_4; 
  ]

let command_to_string command = 
  match command with
  | Move (c1,c2) -> "move" ^ (fst c1) ^ (snd c1) ^ "to" ^ (fst c2) ^ (snd c2)
  | Capture (c1,c2) ->  "move" ^ (fst c1) ^ (snd c1) ^ "to" ^ (fst c2) ^(snd c2)
  | Hint -> "hint"
  | Board -> "board"
  | Quit -> "quit"
  | Help -> "help"
  | Score -> "score"

let make_command_test 
    (name : string)
    (input : string) 
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse input) ~printer:command_to_string)


let command_tests =
  [
    make_command_test "move1" "    move   a3 to b4  " 
      (Move (("a","3"),("b","4")));
    make_command_test "move2" "move   c3 to b4  " 
      (Move (("c","3"),("b","4")));
    make_command_test "cap1" "    capture   a3 to b4  " 
      (Capture (("a","3"),("b","4")));
    make_command_test "hint1" "    hint     " 
      Hint;
    make_command_test "board1" "   board " 
      (Board);
    make_command_test "quit1" "   quit " 
      (Quit);
    make_command_test "help1" "   help " 
      (Help);
    make_command_test "score1" "   score " 
      (Score);
  ]

let bad_command_output str =
  match parse str with
  | exception Malformed -> "Malformed"
  | exception Empty -> "Empty"
  | _ -> "Uncaught"

let make_command_exception_test
    (name : string)
    (input : string) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (bad_command_output input))

let command_exception_tests =
  [
    make_command_exception_test "exp1" "quit asdflk;sdf" 
      "Malformed";
    make_command_exception_test "exp2" "quitt " 
      "Malformed";
    make_command_exception_test "exp3" "    " 
      "Empty";
    make_command_exception_test "exp4" "" 
      "Empty";  
    make_command_exception_test "exp5" "move a a" 
      "Malformed";
    make_command_exception_test "exp6" "move asdfka" 
      "Malformed"; 
    make_command_exception_test "exp7" "move a a a" 
      "Malformed";
    make_command_exception_test "exp8" "move a a a a" 
      "Malformed";
    make_command_exception_test "exp9" "capture a a" 
      "Malformed";
    make_command_exception_test "exp10" "capture a a a" 
      "Malformed";
    make_command_exception_test "exp11" "capture" 
      "Malformed";
    make_command_exception_test "exp12" "captur" 
      "Malformed";
    make_command_exception_test "exp13" "hint a3" 
      "Malformed";
    make_command_exception_test "exp14" "hint a b c" 
      "Malformed";
    make_command_exception_test "exp15" "help a " 
      "Malformed";
    make_command_exception_test "exp16" "score a b c" 
      "Malformed";
  ]


let cant_move_1 : State.state =
  (true, [(("h", "1"), {color = "red"; king = false}); 
          (("g", "2"), {color = "white"; king = false});
          (("f", "3"), {color = "white"; king = false})])
let cant_move_2 : State.state =
  (true, [(("e", "4"), {color = "red"; king = true}); 
          (("c", "6"), {color = "white"; king = false});
          (("g", "6"), {color = "white"; king = false});
          (("d", "5"), {color = "white"; king = false});
          (("f", "5"), {color = "white"; king = false});
          (("d", "3"), {color = "white"; king = false});
          (("f", "3"), {color = "white"; king = false});
          (("c", "2"), {color = "white"; king = false});
          (("g", "2"), {color = "white"; king = false});])
let can_move_1 : State.state =
  (true, [(("h", "1"), {color = "red"; king = false}); 
          (("g", "2"), {color = "white"; king = false})])
let can_move_2 : State.state =
  (true, [(("h", "1"), {color = "red"; king = false}); 
          (("f", "3"), {color = "white"; king = false})])

let make_cant_move_test     
    (name : string)
    (state : State.state)
    (expected_output : bool) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (cant_move state))

let cant_move_tests = 
  [
    make_cant_move_test "cant move 1" cant_move_1 true;
    make_cant_move_test "cant move 2" cant_move_2 true;
    make_cant_move_test "can move 1" can_move_1 false;
    make_cant_move_test "can move 2" can_move_2 false;
  ]

let can_capture : State.state = 
  (true, [(("f", "3"), {color = "red"; king = false}); 
          (("e", "4"), {color = "white"; king = false})])
let can_move_no_cap : State.state = 
  (true, [(("c", "4"), {color = "red"; king = false})])
let can_move_no_cap2 : State.state = 
  (false, [(("b", "5"), {color = "red"; king = false})])

let must_cap coord_from coord_to state = 
  try 
    vmmc coord_from coord_to state
  with MustCap -> state

let make_vmmc_test     
    (name : string)
    (coord_from : coordinate)
    (coord_to : coordinate)
    (state : state)
    (expected_output : state) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (vmmc coord_from coord_to state))

let vmmc_tests = 
  [
    make_vmmc_test "can move no cap" ("c", "4") ("b", "5") can_move_no_cap 
      can_move_no_cap2;
  ]

let two_piece_board : State.state = 
  (false, [(("f", "3"), {color = "red"; king = false}); 
           (("g", "4"), {color = "white"; king = false})])
let two_after : State.state =
  (true, [(("e", "2"), {color = "white"; king = false})])

let make_easy_AI_test  
    (name : string)
    (state : State.state)
    (expected_output : State.state) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (easyAI state))

let easyAI_tests =
  [
    make_easy_AI_test "little" two_piece_board two_after
  ]

let suite =
  "test suite for checkers"  >::: List.flatten [
    valid_move_tests;
    move_tests;
    valid_capture_tests;
    capture_tests;
    command_tests;
    command_exception_tests;
    cant_move_tests;
    vmmc_tests;
    easyAI_tests
  ]

let _ = run_test_tt_main suite
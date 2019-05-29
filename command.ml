(* Command *)

type coordinate = string * string 

type command = 
  | Move of (coordinate * coordinate)  
  | Capture of (coordinate * coordinate)
  | Hint
  | Board 
  | Quit
  | Help
  | Score 

exception Empty

exception Malformed

(** [condense] takes a list of type [string list], and returns a copy of that 
    list with all of the empty string elements removed. *)
let rec condense = function
  | [] -> []
  | h::t -> if h = "" then condense t else h :: condense t

(** [parse_move tail] removes the word "to" from the tokenized move command and
    returns a tuple of coordinates *)
let parse_move tail =
  let filtered_tail = List.filter (fun elt -> elt <> "to") tail in
  if List.length filtered_tail <> 2 then raise Malformed else
    let coordinate1 = List.hd filtered_tail in
    let coordinate2 = List.nth filtered_tail 1 in
    (((String.make 1 coordinate1.[0]), (String.make 1 coordinate1.[1])), 
     ((String.make 1 coordinate2.[0]), (String.make 1 coordinate2.[1])))

let parse str = 
  let string_list = String.split_on_char ' ' str in
  let s = condense string_list in
  match s with 
  | [] -> raise Empty
  | h::t -> if (h <> "move" && h <> "hint" && h <> "board" && h <> "quit" && 
                h <> "score" && h <> "capture" && h <> "help")
            || (h = "quit" && t <> []) 
            || (h = "move" && t = []) 
            || (h = "board" && t <> [])
            || (h = "score" && t <> [])
            || (h = "help" && t <> [])
            || (h = "hint" && t <> [])
            || (h  = "move" && (List.length t) <> 3)
            || (h  = "capture" && (List.length t) <> 3)
    then raise Malformed
    else (
      if h = "move" then Move (parse_move t)
      else if h = "capture" then Capture (parse_move t)
      else if h = "hint" then Hint
      else if h = "board" then Board
      else if h = "score" then Score
      else if h = "help" then Help
      else Quit  )
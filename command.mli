(** Parsing of player commands. *)

(** The type [coordinate] represents the coordinate that can be part of a 
    player command.  The first element of the tuple represents the column the
    checkerboard and the second element of the tuple represents the row of the
    checkerboard. For example:
    - If the player command is ["Move b3 to a4"], then the coordinates are  
      [("b","3")] and ["a","4"].
    A [coordinate] is not permitted to be a tuple of empty strings. *)
type coordinate = string * string 

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a tuple of coordinates. *)
type command = 
  | Move of (coordinate * coordinate) 
  | Capture of (coordinate * coordinate)  
  | Hint
  | Board 
  | Quit
  | Help
  | Score

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become a tuple of coordinates.
    Examples: 
    - [parse "    move   a3 to b4  "] is [Move (("a","3"),("b","4"))]
    - [parse "quit"] is [Quit]. 
    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).
    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is none of the commands,
    or if the verb is "quit", "board", "help", or "score" and there is a 
    non-empty object phrase, or if the verb is "move", "capture", or "hint" and 
    there are empty coordinates.*)
val parse : string -> command
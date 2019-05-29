# OCaml Checkers
OCaml Checkers is a single or two-player game of checkers playable in the command line and built with OCaml. It uses a text-based user interface that displays the checkers board using ASCII characters and emojis. Pieces are controlled by using typed commands, and the game also features a `hint` command for new players that uses the game's hard AI to suggest a strong move for the player to make.

## How to Play
1. Clone this repository.
2. `cd` into the cloned repository.
3. Run `make play` and follow the game instructions in the terminal.

## Game Modes
OCaml Checkers has two game modes: single or two-player checkers.

### Single Player
A player can choose to play against an easy, medium, or hard difficulty checkers AI.

- The easy AI calculates the possible moves it can make and simply chooses the first one available.
- The medium AI randomly chooses a move from a list of all possible moves it can make.
- The hard AI is not so simplistic. It uses a complex point scoring system that evaluates the strength of a board state and uses this point system to determine the best move to make. It does this in two steps: it first evaluates the strength of a board state after making a move, and then it also assumes the opponent chooses the best possible response to the AI's move and evaluates the strength of the resulting board state. The AI uses this two-fold point calculation to choose the best move available to it.

### Two Player
This game mode lets two players at the same computer play a game of checkers against each other in the terminal!

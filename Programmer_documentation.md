# Modules

The modules in this program depend each other in this topological ordering:
Board -> Moves -> Input -> Player -> main -> Tests

## Description of modules:

Board: Implements the Board type and functions related to it, like searching for unoccupied spaces, and the displaying of the board to the console.

Moves: Implements the Move and GameState types, handles checking the legality of moves and changing gamestate after a move.

Input: Handles input for human players.

Player: Contains the various types of players and the minimax algorithm for HeuristicAI, a Player entity gets a function call from main and returns a chosen move.

main: Contains the playGame function that the user should interface with. Calls on Player entities to provide moves, handles game end.

Tests: Contains miscellaneous functions for testing the other modules, is not necessary for the program to run.

# Design decisions

## BoardField
The BoardField type is constructed with "Maybe Piece", where Piece is Black or White. It would be possible to add a "Nothing" constructor to Piece to represent empty spaces instead of using the Maybe monad, but I think this way the semantics of the situation are better captured - "Nothing" isn't a type of piece in reality.

On the other hand, I had to convert between BoardField and Piece in many places in the program, so it might have been simpler to have these as one type.

## Board
Board is a list of rows, where each row is a list. It would be possible to use a matrix library for a proper 2D matrix, but I wanted to keep the program as simple as possible. The code doesn't check anywhere that the board is rectangular, and there will likely be bugs if you create a non-rectangular board - I don't think that anyone will want to extend this to a non-rectangular board, all Fangqi variants are played on rectangular boards.

However, I did account for trying to access coordinates outside of the board, like (-1,-2), whether that comes from user input or a function trying to get the neighbors of a square on the edge of the board. It is first checked whether the coordinates are in bounds before using the !! operator, which may throw an exception.

## Phases
Unlike the Fangqi variant described by the Wikipedia, the implemented variant lets a player capture stones immediately when forming a square in the drop phase. This results in a slightly different game, but I chose this implementation to avoid adding a further component to GameState (the number of stones to be captured)

## getNonEmptyNeighborCount, getMoveHeuristic, getCaptureHeuristic
One of the main heuristics used in the minimax algorithm used for evaluating how promising moves are. This is needed to decide which moves and responses should be searched - otherwise the AI is, even on small depths, unbearably slow on a 7x7 board with over 40 moves in each position in the beginning, and possibly even a larger number of moves available in the endgame. These heuristics essentially say that it is good to move to positions and capture enemy stones on positions surrounded by a lot of stones of any color. This follows from the fact that both the player and their opponent have to put pieces together to make squares and capture enemies.

## Captures
Some variants of Fangqi allow capturing multiple stones if more squares are formed at once, but this is a rare event and would be difficult for the AI to "understand", so the program allows capturing at most one stone after a move.

## Move, Capture, MoveCapture
A MoveCapture encapsulates the Move and the (maybe) following capture. Initially I wrote it such that a move would be one Move and the subsequent capture would be another Move (with the Remove constructor), but this became difficult when I started writing the minimax algorithm, because it meant that a move which resulted in a capture spanned two depths in the DFS tree, so I introduced Capture and MoveCapture.

## requestMoveUntilGot
I thought about an input format like "a1 a2" (on one line) for shift moves, but I decided that the two-prompt way of doing it is fast enough and maybe simpler for the user to understand.

I could allow whitespaces around the input, but it didn't seem like a necessary improvement - the user will be likely typing slowly when inputting moves - and it would add complexity to the parser.

## Player
Originally the Player interface required separate chooseMove and chooseCapture functions, but I realized they weren't necessary anywhere after creating the MoveCapture type that contained an optional capture, so I removed them.


## minimaxMoveGetter
This is a "wrapper" for the real minimax function *minimaxGetBestMove*. It handles the situation when an end state is reached in the search. The dummy move should not surface anywhere in the game, because the game should check for an end state before the HeuristicAI would have the opportunity to select the dummy move (because it does not have any other moves to make). If the HeuristicAI does choose it anyway, an exception should occur because there is a check for the players, including AI, attempting illegal moves.

## heuristic
This is the second type of heuristic used (after getMoveHeuristic), which is used to evaluate states when the depth limit of a minimax search is reached. In accordance with usual game theory notation, it says how good a position is for White, no matter whose turn it is - so White is the maximizing player and Black is the minimizing player.

The value is a simple ratio of stones. Potential refinements could account for the clustering or centrality of a player's stones (which adds to their value), or the number of different moves a player could make if it were their turn (increasing the probability that they can make a good move).

## evaluateEndPosition
This simply checks if a player can't move. If a player doesn't have pieces (the usually mentioned end game condition), the getPossibleMoves/getPossibleMCs function will say that they don't have any moves, so this state is automatically accounted for as well.
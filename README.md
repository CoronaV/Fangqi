# Nonprocedural programming credit project Fangqi

This repository implements the game Fangqi (方棋) in Haskell. Fangqi is a Chinese board game similar to Nine Men's Morris. For details see https://en.wikipedia.org/wiki/Fangqi.

To play a game, load the main.hs module and run the function "playGame".
The first two arguments represent players. The first player listed will play White, the second Black. White moves first. Available players are DummyAI (plays bad deterministic moves fast, for testing), HeuristicAI (uses the minimax algorithm with heuristics) and Human (prompts the console for moves).
The third argument are board size parameters. The board is a i by j rectangle. Board size isn't hardcoded, since variants of Fangqi have many different board sizes.
Use example: "playGame Human HeuristicAI (7,7)"
 
## Interface
The program prints the board in this format:

```
7|.......
6|.ww....
5|.......
4|....b..
3|.w.bb..
2|.......
1|.......
 --------
  abcdefg
```

A dot "." represents an empty space, "w" is a white piece and "b" is a black piece.
The left and bottom edge are labelled with coordinates that are used to input moves and report moves from the AI.

The game is divided into three phases:
1) PhaseDrop, where players alternate in dropping stones on the board until it is full. In contrast to the main variant described in the Wikipedia articles, this implementation performs captures immediately when a square is formed instead of at the end of the phase.
2) PhaseRemove, where players each remove one stone of the opponent.
3) PhaseShift, where players take turns moving one own stone any number of free spaces horizontally or vertically, trying to form squares.

## Input

To input a move for a human player: In PhaseDrop or PhaseRemove, type in one coordinate to drop/remove a stone at the coordinate. The input must look like "b3" or "3b", indicating a square according to the labelled edges of the board. In PhaseShift, a human player will be prompted to put in two sets of coordinates, first the square to move a stone from, and then the square to move a stone to.

If a human makes a move that allows a capture (forming a 2x2 square of stones), they will be prompted for a set of coordinates of the enemy stone to be captured.

If a player cannot move (whether they have any stones remaining or not), the game will declare a win for their opponent. 


## License
This program is provided under the MIT License.

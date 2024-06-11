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
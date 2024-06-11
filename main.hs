import Data.Maybe (isNothing)
import Debug.Trace (trace)
import Board
    ( Board, BoardField(..), Piece(..), emptyBoard, GameState (..), Phase (..), displayGameState, unpackGSToBoard )
import Player (Player (..), RandomAI (..), Human (..))
import Moves (switchColor, getSpaceOfType, getSpaceTypeNumber, MoveCapture (..), checkLegalAndResolveMC, phaseEndCondition, gamestateCheckPhaseEnd, nextPhase, gamestateCheckGameEnd)


-- switchTurn :: GameState -> GameState
-- switchTurn (GameState b piece phase) = GameState b (switchColor piece) phase

-- > phase end conditions where
-- move function of player1 -> move function of player2 -> initial state -> final state

-- improve this function/build on this function
-- so it can be used to play the complete 3-phase game
-- and for simulating the minimax algorithm

-- use "do" here to prevent the IO monad from getting everywhere (like move validity checking where it's not appropriate)
playGameInit :: (Player p1, Player p2) => p1 -> p2 -> (Int,Int) -> IO ()
playGameInit p1 p2 (i,j) = do
    displayGameState startState
    playGame p1 p2 startState
    where startState = GameState (emptyBoard i j) White PhaseDrop

playGame :: (Player p1, Player p2) => p1 -> p2 -> GameState -> IO ()
playGame p1 p2 current = do
    let gameEnded = gamestateCheckGameEnd current -- phase ends are handled by checkLegalAndResolveMC
    if gameEnded
        then do
        putStrLn "End of game. The result is:"
        putStrLn (evaluateEndPosition current)
    else do
        chosenMoveCapture <- chooseMoveCapture p1 current
        let afterMoveCapture = checkLegalAndResolveMC current chosenMoveCapture
        displayGameState afterMoveCapture
        playGame p2 p1 afterMoveCapture


-- playGameWithHistory :: (Player p1, Player p2) => p1 -> p2 -> (Int,Int) -> IO ()
-- playGameWithHistory =  do
--     putStrLn "Start of game, the players take turns dropping pieces"
--     displayGameState startState
--     dropEnd <- playPhase p1 p2 startState
--     putStrLn "End of drop phase, now each player removes one piece"
--     removeEnd <- playPhase p1 p2 (nextPhase dropEnd)
--     putStrLn "End of remove phase, now players take turn shifting a piece any number of free squares vertically or horizontally."
--     shiftEnd <- playPhase p1 p2 (nextPhase removeEnd)
--     putStrLn "End of game. The result is:"
--     putStrLn (evaluateEndPosition shiftEnd)
--     return ()
--         where startState = GameState (emptyBoard i j) White PhaseDrop


-- (Int,Int) = size of board
-- playGame :: (Player p1, Player p2) => p1 -> p2 -> (Int,Int) -> IO ()
-- playGame p1 p2 (i,j) = do
--     putStrLn "Start of game, the players take turns dropping pieces"
--     displayGameState startState
--     dropEnd <- playPhase p1 p2 startState
--     putStrLn "End of drop phase, now each player removes one piece"
--     removeEnd <- playPhase p1 p2 (nextPhase dropEnd)
--     putStrLn "End of remove phase, now players take turn shifting a piece any number of free squares vertically or horizontally."
--     shiftEnd <- playPhase p1 p2 (nextPhase removeEnd)
--     putStrLn "End of game. The result is:"
--     putStrLn (evaluateEndPosition shiftEnd)
--     return ()
--         where startState = GameState (emptyBoard i j) White PhaseDrop


evaluateEndPosition :: GameState -> String
evaluateEndPosition gs = do
    let gameEnded = gamestateCheckGameEnd gs
    if gameEnded then do
        let winner = show (switchColor (piece gs)) -- the player that should move can't move -> win for the other player
        winner ++ " won the game!"
    else do
        error "Game ended prematurely."
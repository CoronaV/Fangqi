import Data.Maybe (isNothing)
import Debug.Trace (trace)
import Board
    ( Board, BoardField(..), Piece(..), emptyBoard, GameState (..), Phase (..), displayGameState, unpackGSToBoard )
import Player (Player (..), RandomAI (..), Human (..))
import Moves (switchColor, getSpaceOfType, getSpaceTypeNumber, MoveCapture (..), checkLegalAndResolveMC, phaseEndCondition, gamestateCheckPhaseEnd, nextPhase)


-- switchTurn :: GameState -> GameState
-- switchTurn (GameState b piece phase) = GameState b (switchColor piece) phase

-- > phase end conditions where
-- move function of player1 -> move function of player2 -> initial state -> final state

-- improve this function/build on this function
-- so it can be used to play the complete 3-phase game
-- and for simulating the minimax algorithm

-- use "do" here to prevent the IO monad from getting everywhere (like move validity checking where it's not appropriate)
playPhase :: (Player p1, Player p2) => p1 -> p2 -> GameState -> IO GameState
playPhase p1 p2 current = do
    let ended = gamestateCheckPhaseEnd current
    if ended
        then return current
    else do
        chosenMoveCapture <- chooseMoveCapture p1 current
        let afterMoveCapture = checkLegalAndResolveMC current chosenMoveCapture
        displayGameState afterMoveCapture
        playPhase p2 p1 afterMoveCapture

-- (Int,Int) = size of board
playGame :: (Player p1, Player p2) => p1 -> p2 -> (Int,Int) -> IO ()
playGame p1 p2 (i,j) = do
    putStrLn "Start of game, the players take turns dropping pieces"
    displayGameState startState
    dropEnd <- playPhase p1 p2 startState
    putStrLn "End of drop phase, now each player removes one piece"
    removeEnd <- playPhase p1 p2 (nextPhase dropEnd)
    putStrLn "End of remove phase, now players take turn shifting a piece any number of free squares vertically or horizontally."
    shiftEnd <- playPhase p1 p2 (nextPhase removeEnd)
    putStrLn "End of game. The result is:"
    putStrLn (evaluateEndPosition shiftEnd)
    return ()
        where startState = GameState (emptyBoard i j) White PhaseDrop


--TODO: add captures to the drop phase! work on this since this will be required in the shift phase too
-- some Fangqi variations have the players count the squares and remove pieces all at once at the end of the drop phase,
-- but it might be easier to just remove a piece immediately as in the shift phase?

evaluateEndPosition :: GameState -> String
evaluateEndPosition (GameState b piece _)
    | isNothing $ getSpaceOfType b (BoardField $ Just White) = "Black won the game!" --there are no white pieces left
    | isNothing $ getSpaceOfType b (BoardField $ Just Black) = "White won the game!"
    | otherwise = "The game is drawn!"
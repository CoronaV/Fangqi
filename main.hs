import Data.Maybe (isNothing, fromMaybe)
import Debug.Trace (trace)
import Board
    ( Board, BoardRow, BoardField(..), Piece(..), boardRows, emptyBoard, GameState (..), Phase (..), isLeftUpCornerOfSquare, isAnyCornerOfSquare, displayGameState )
import Player (Player (..), RandomAI (..), Human (..))
import Moves (Move(..),checkLegalAndResolve, switchColor, getSpaceOfType, getSpaceTypeNumber, MoveCapture (..), checkLegalAndResolveMC)


-- switchTurn :: GameState -> GameState
-- switchTurn (GameState b piece phase) = GameState b (switchColor piece) phase

-- > phase end conditions where
-- move function of player1 -> move function of player2 -> end eval function -> initial state -> final state

-- improve this function/build on this function
-- so it can be used to play the complete 3-phase game
-- and for simulating the minimax algorithm

-- use "do" here to prevent the IO monad from getting everywhere (like move validity checking where it's not appropriate)
playPhase :: (Player p1, Player p2) => p1 -> p2 -> (GameState -> Bool) -> GameState -> IO GameState
playPhase p1 p2 endCondition current = do
    let ended = endCondition current
    if ended
        then return current
    else do
        chosenMoveCapture <- chooseMoveCapture p1 current
        let afterMoveCapture = checkLegalAndResolveMC current chosenMoveCapture
        displayGameState afterMoveCapture
        playPhase p2 p1 endCondition afterMoveCapture
    -- | endCondition current = current
    -- | otherwise = playPhase p1 p2 endCondition (checkLegalAndResolve current (makeMove p1 current))



-- (Int,Int) = size of board
playGame :: (Player p1, Player p2) => p1 -> p2 -> (Int,Int) -> IO ()
playGame p1 p2 (i,j) = do
    putStrLn "Start of game, the players take turns dropping pieces"
    dropEnd <- playPhase p1 p2 dropPhaseEndCheck (GameState (emptyBoard i j) White PhaseDrop )
    putStrLn "End of drop phase, now each player removes one piece"
    removeEnd <- playPhase p1 p2 removePhaseEndCheck (nextPhase dropEnd)
    putStrLn "End of remove phase, now players take turn shifting a piece any number of free squares vertically or horizontally."
    shiftEnd <- playPhase p1 p2 shiftPhaseEndCheck (nextPhase removeEnd)
    putStrLn "End of game. The result is:"
    putStrLn (evaluateEndPosition shiftEnd)
    return ()


-- players stop dropping stones when the board is filled
dropPhaseEndCheck :: GameState -> Bool
dropPhaseEndCheck (GameState b _ _) = isNothing $ getSpaceOfType b (BoardField Nothing)


nextPhase :: GameState -> GameState
nextPhase (GameState b piece PhaseDrop) = GameState b piece PhaseRemove
nextPhase (GameState b piece PhaseRemove) = GameState b piece PhaseShift


-- 2nd phase: each player removes one of their opponent's stones
-- players remove one stone each -> there should be two empty spaces
removePhaseEndCheck :: GameState -> Bool
removePhaseEndCheck (GameState b _ _) = getSpaceTypeNumber b (BoardField Nothing) >= 2


-- make phase a class with this as method? or merge the endCheck methods into one since GameState contains Phase info?
-- the game ends if there are no white or no black stones
-- also TODO: draw on repetition of position, requires keeping history
shiftPhaseEndCheck :: GameState -> Bool
shiftPhaseEndCheck (GameState b _ _) = getSpaceTypeNumber b (BoardField $ Just White) == 0 || getSpaceTypeNumber b (BoardField $ Just Black) == 0


--TODO: add captures to the drop phase! work on this since this will be required in the shift phase too
-- some Fangqi variations have the players count the squares and remove pieces all at once at the end of the drop phase,
-- but it might be easier to just remove a piece immediately as in the shift phase?

evaluateEndPosition :: GameState -> String
evaluateEndPosition (GameState b piece _)
    | isNothing $ getSpaceOfType b (BoardField $ Just White) = "Black won the game!" --there are no white pieces left
    | isNothing $ getSpaceOfType b (BoardField $ Just Black) = "White won the game!"
    | otherwise = "The game is drawn!"
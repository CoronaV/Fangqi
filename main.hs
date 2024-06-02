import Data.Maybe (isNothing, fromMaybe)
import Debug.Trace (trace)
import Board
    ( Board, BoardRow, BoardField(..), Piece(..), boardRows, emptyBoard, GameState (..), Phase (..), isLeftUpCornerOfSquare, isAnyCornerOfSquare, displayGameState )
import Player (Player (..), RandomAI (..), Human (..))
import Moves (Move(..),checkLegalAndResolve, switchColor, getSpaceOfType, getSpaceTypeNumber, checkCapture, MoveCapture (..), checkLegalAndResolveMC)


switchTurn :: GameState -> GameState
switchTurn (GameState b piece phase) = GameState b (switchColor piece) phase

-- > phase end conditions where
-- move function of player1 -> move function of player2 -> end eval function -> initial state -> final state

-- use "do" here to prevent the IO monad from getting everywhere (like move validity checking where it's not appropriate)
playGame :: (Player p1, Player p2) => p1 -> p2 -> (GameState -> Bool) -> GameState -> IO GameState
playGame p1 p2 endCondition current = do
    let ended = endCondition current
    if ended
        then return current
    else do
        chosenMoveCapture <- chooseMoveCapture p1 current
        let afterMoveCapture = checkLegalAndResolveMC current chosenMoveCapture
        displayGameState afterMoveCapture
        playGame p2 p1 endCondition (switchTurn afterMoveCapture)
    -- | endCondition current = current
    -- | otherwise = playGame p1 p2 endCondition (checkLegalAndResolve current (makeMove p1 current))


-- players stop dropping stones when the board is filled
dropPhaseEndCheck :: GameState -> Bool
dropPhaseEndCheck (GameState b _ _) = isNothing $ getSpaceOfType b (BoardField Nothing)


nextPhase :: GameState -> GameState
nextPhase (GameState b piece PhaseDrop) = GameState b piece PhaseRemove
nextPhase (GameState b piece PhaseRemove) = GameState b piece PhaseShift
nextPhase (GameState b piece PhaseShift) = GameState b piece PhaseShift --TODO


-- 2nd phase: each player removes one of their opponent's stones
-- players remove one stone each -> there should be two empty spaces
removePhaseEndCheck :: GameState -> Bool
removePhaseEndCheck (GameState b _ _) = getSpaceTypeNumber b (BoardField Nothing) >= 2




-- make phase a class with this as method? or merge the endCheck methods into one since GameState contains Phase info?
-- the game ends if there are no white or no black stones
-- also TODO: draw on repetition of position, requires keeping history
shiftPhaseEndCheck :: GameState -> Bool
shiftPhaseEndCheck (GameState b _ _) = getSpaceTypeNumber b (BoardField $ Just White) == 0 || getSpaceTypeNumber b (BoardField $ Just Black) == 0

-- sampleGameShiftPhase :: IO GameState
-- sampleGameShiftPhase = playGame RandomAI RandomAI shiftPhaseEndCheck (nextPhase sampleGameRemovePhase) --TODO


--TODO: add captures to the drop phase! work on this since this will be required in the shift phase too
-- some Fangqi variations have the players count the squares and remove pieces all at once at the end of the drop phase,
-- but it might be easier to just remove a piece immediately as in the shift phase?

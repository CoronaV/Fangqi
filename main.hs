import Data.Maybe (isNothing, fromMaybe)
import Debug.Trace (trace)
import Board
    ( Board, BoardRow, BoardField(..), Piece(..), boardRows, emptyBoard, GameState (..), Phase (..), isLeftUpCornerOfSquare, isAnyCornerOfSquare, displayGameState )
import Player (Player (..), RandomAI (..), Human (..))
import Moves (Move(..),checkLegalAndResolve, switchColor, getSpaceOfType, getSpaceTypeNumber, checkCapture)
import GHC.Utils.Misc (count)
import Control.Applicative (liftA2)


--arguments: initial position -> first player to move -> final position
-- dropPhase :: Board -> Piece -> Board
-- dropPhase b playerTurn
--     | not $ dropPhaseEndCheck b = dropPhase (playArbitraryDrop b (Just playerTurn)) (changeTurn playerTurn)
--     | otherwise = b --return the board at the end of the drop phase


-- -- 2nd phase: each player removes one of their opponent's stones
-- removePhase :: Board -> Piece -> Board
-- removePhase b playerTurn = playArbitraryRemove bAfterFirstRemove (changeTurn playerTurn) playArbitraryRemove
--     where bAfterFirstRemove = playArbitraryRemove b playerTurn playArbitraryRemove


switchTurn :: GameState -> GameState
switchTurn (GameState b piece phase) = GameState b (switchColor piece) phase

-- > phase end conditions where
-- move function of player1 -> move function of player2 -> end eval function -> initial state -> final state

-- TODO: use "do" here to prevent the IO monad from getting everywhere (like move validity checking where it's not appropriate)
playGame :: (Player p1, Player p2) => p1 -> p2 -> (GameState -> Bool) -> GameState -> IO GameState
playGame p1 p2 endCondition current = do
    let ended = endCondition current
    if ended
        then return current
    else do
        chosenMove <- chooseMove p1 current
        let newState = checkLegalAndResolve current chosenMove  -- fix the board being drawn again if the move was illegal
        let mayCapture = checkCapture newState chosenMove
        -- chosenMove <- fmap (chooseMove p1) current
        -- newState <- liftA2 checkLegalAndResolve current chosenMove
        -- mayCapture <- fmap (checkCapture newState) chosenMove
        if mayCapture
            then do
                -- importantly the color to play remains the same
                -- until after the potential capture is resolved
                putStrLn "A capture is possible!"
                -- display the board again...
                captureMove <- chooseCapture p1 newState
                afterCapture <- return $ checkLegalAndResolve newState captureMove
                --print the new board
                displayGameState afterCapture

                -- swap players!
                playGame p2 p1 endCondition (switchTurn afterCapture)
            else do
                --print the new board
                displayGameState newState
                playGame p2 p1 endCondition (switchTurn newState)
        --current
        --playGame p1 p2 endCondition (checkLegalAndResolve current (makeMove p1 current))
    -- | endCondition current = current
    -- | otherwise = playGame p1 p2 endCondition (checkLegalAndResolve current (makeMove p1 current))



-- move to board.hs TODO
-- deal with edges of the board
-- getNeighborCoords :: Board -> (Int, Int) -> [(Int, Int)]
-- getNeighborCoords b (i,j)= [(i,j),_]


--get user input ->MoveType

--debugging:
chessBoard :: Board
chessBoard = emptyBoard 8 8

startState :: GameState
startState = GameState chessBoard White PhaseDrop

-- players stop dropping stones when the board is filled
dropPhaseEndCheck :: GameState -> Bool
dropPhaseEndCheck (GameState b _ _) = isNothing $ getSpaceOfType b (BoardField Nothing)

sampleGameDropPhase :: IO GameState
sampleGameDropPhase = playGame RandomAI RandomAI dropPhaseEndCheck startState

nextPhase :: GameState -> GameState
nextPhase (GameState b piece PhaseDrop) = GameState b piece PhaseRemove
nextPhase (GameState b piece PhaseRemove) = GameState b piece PhaseShift
nextPhase (GameState b piece PhaseShift) = GameState b piece PhaseShift --TODO

-- players remove one stone each -> there should be two empty spaces
removePhaseEndCheck :: GameState -> Bool
removePhaseEndCheck (GameState b _ _) = getSpaceTypeNumber b (BoardField Nothing) >= 2


sampleGameRemovePhase :: IO GameState
sampleGameRemovePhase = do
    start <- sampleGameDropPhase
    playGame RandomAI RandomAI removePhaseEndCheck (nextPhase start)

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

humanGameDropPhase :: IO GameState
humanGameDropPhase = playGame Human Human dropPhaseEndCheck startState

humanAIGameDropPhase :: IO GameState
humanAIGameDropPhase = playGame Human RandomAI dropPhaseEndCheck startState
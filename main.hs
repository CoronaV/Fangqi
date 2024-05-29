import Data.Maybe (isNothing, fromMaybe)
import Debug.Trace (trace)
import Board
    ( Board, BoardRow, BoardField(..), Piece(..), boardRows, emptyBoard, GameState (..), Phase (..) )
import Player ()
import Moves (Move(..),checkLegalAndResolve, changeTurn)


chessBoard = emptyBoard 8 8

-- functions related to making moves
-- there are 2 types of moves: placing a stone and moving a stone to a neighboring intersection (=field,square)

-- a placing move is legal if the intersection was empty before the move


type FlattenedBoard = [(BoardField, Int, Int)]

addCoordsToRow :: BoardRow -> Int -> FlattenedBoard
addCoordsToRow row rowNum = zip3 row (replicate rowL rowNum) [0..rowL]
    where rowL = length row

flattenWithCoords :: Board -> FlattenedBoard
flattenWithCoords b = concat $ zipWith addCoordsToRow b [0..boardRows b]


-- players stop dropping stones when the board is filled
dropPhaseEndCheck :: Board -> Bool
dropPhaseEndCheck b = isNothing $ getSpaceOfType b (BoardField Nothing)

-- have the computer take turns until the board is filled




--arguments: initial position -> first player to move -> final position
-- dropPhase :: Board -> Piece -> Board
-- dropPhase b playerTurn
--     | not $ dropPhaseEndCheck b = dropPhase (playArbitraryDrop b (Just playerTurn)) (changeTurn playerTurn)
--     | otherwise = b --return the board at the end of the drop phase


-- -- 2nd phase: each player removes one of their opponent's stones
-- removePhase :: Board -> Piece -> Board
-- removePhase b playerTurn = playArbitraryRemove bAfterFirstRemove (changeTurn playerTurn) playArbitraryRemove
--     where bAfterFirstRemove = playArbitraryRemove b playerTurn playArbitraryRemove



-- > phase end conditions where
-- move function of player1 -> move function of player2 -> end eval function -> initial state -> final state
playGame :: (Player p1, Player p2) => p1 -> p2 -> (GameState -> Bool) -> GameState -> GameState
playGame p1 p2 endCondition current
    | endCondition current = current
    | otherwise = playGame p1 p2 endCondition (checkLegalAndResolve current (makeMove p1 current))


-- randomPlayer :: Player
-- randomPlayer gs = _

-- "player" is an interface with a method for choosing moves
class Player p where
    makeMove :: p -> GameState -> Move

data RandomAI = RandomAI


instance Player RandomAI where
    makeMove :: RandomAI -> GameState -> Move
    -- if there is no legal move, we messed up, so the AI will just make whatever move and cause an exception down the line
    -- play on the first free field from top left, if there is any
    makeMove RandomAI (GameState b piece PhaseDrop) = Drop piece $ fromMaybe (0,0) (getSpaceOfType b (BoardField Nothing) )
    makeMove RandomAI (GameState b piece PhaseRemove) = Remove piece $ fromMaybe (0,0) (getSpaceOfType b (BoardField (Just $ changeTurn piece)) )
    makeMove RandomAI (GameState b piece PhaseShift) = Shift piece (0,0) (0,0) --TODO


--retire this in favor of isLegalMove and isLegalAnywhereMove?
-- gets *any* space of the type for the random AI and for checking phase end conditions
getSpaceOfType :: Board -> BoardField -> Maybe (Int, Int)
getSpaceOfType b = getSpaceOfType' $ flattenWithCoords b
    where
        getSpaceOfType' :: FlattenedBoard -> BoardField -> Maybe (Int, Int)
        getSpaceOfType' [] _ = Nothing
        getSpaceOfType' ((field, row, col):fields) fieldtype
            | field == fieldtype = Just (row, col)
            | otherwise = getSpaceOfType' fields fieldtype



--get user input ->MoveType

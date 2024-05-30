import Data.Maybe (isNothing, fromMaybe)
import Debug.Trace (trace)
import Board
    ( Board, BoardRow, BoardField(..), Piece(..), boardRows, emptyBoard, GameState (..), Phase (..), isLeftUpCornerOfSquare, isAnyCornerOfSquare )
import Player ()
import Moves (Move(..),checkLegalAndResolve, changeTurn)
import GHC.Utils.Misc (count)




-- functions related to making moves
-- there are 2 types of moves: placing a stone and moving a stone to a neighboring intersection (=field,square)

-- a placing move is legal if the intersection was empty before the move


type FlattenedBoard = [(BoardField, Int, Int)]

addCoordsToRow :: BoardRow -> Int -> FlattenedBoard
addCoordsToRow row rowNum = zip3 row (replicate rowL rowNum) [0..rowL]
    where rowL = length row

flattenWithCoords :: Board -> FlattenedBoard
flattenWithCoords b = concat $ zipWith addCoordsToRow b [0..boardRows b]


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
    makeMove RandomAI (GameState b piece PhaseShift) = Shift piece (0,0) (0,0) --TODO: get one of the player's stones that has a neighboring space free, move it there


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

-- counts the number of spaces of a type on the board
getSpaceTypeNumber :: Board -> BoardField -> Int
getSpaceTypeNumber b fieldType = count (isFieldType fieldType) $ flattenWithCoords b
    where
        isFieldType :: BoardField -> (BoardField, Int, Int) -> Bool
        isFieldType ft (ft2, _, _) = ft == ft2


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

sampleGameDropPhase :: GameState
sampleGameDropPhase = playGame RandomAI RandomAI dropPhaseEndCheck startState

nextPhase :: GameState -> GameState
nextPhase (GameState b piece PhaseDrop) = GameState b piece PhaseRemove
nextPhase (GameState b piece PhaseRemove) = GameState b piece PhaseShift
nextPhase (GameState b piece PhaseShift) = GameState b piece PhaseShift --TODO

-- players remove one stone each -> there should be two empty spaces
removePhaseEndCheck :: GameState -> Bool
removePhaseEndCheck (GameState b _ _) = getSpaceTypeNumber b (BoardField Nothing) >= 2


sampleGameRemovePhase :: GameState
sampleGameRemovePhase = playGame RandomAI RandomAI removePhaseEndCheck (nextPhase sampleGameDropPhase)

-- make phase a class with this as method? or merge the endCheck methods into one since GameState contains Phase info?
-- the game ends if there are no white or no black stones
-- also TODO: draw on repetition of position, requires keeping history
shiftPhaseEndCheck :: GameState -> Bool
shiftPhaseEndCheck (GameState b _ _) = getSpaceTypeNumber b (BoardField $ Just White) == 0 || getSpaceTypeNumber b (BoardField $ Just Black) == 0

sampleGameShiftPhase :: GameState
sampleGameShiftPhase = playGame RandomAI RandomAI shiftPhaseEndCheck (nextPhase sampleGameRemovePhase) --TODO


--TODO: add captures to the drop phase
-- some Fangqi variations have the players count the squares and remove pieces all at once at the end of the drop phase,
-- but it might be easier to just remove a piece immediately as in the shift phase?


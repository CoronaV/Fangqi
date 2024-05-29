import Data.Maybe (isNothing)
import Debug.Trace (trace)
import Board
    ( Board, BoardRow, BoardField(..), Piece(..), boardRows, emptyBoard )


chessBoard = emptyBoard 8 8

-- functions related to making moves
-- there are 2 types of moves: placing a stone and moving a stone to a neighboring intersection (=field,square)

-- a placing move is legal if the intersection was empty before the move
type MoveLegality = Bool

replaceByIndex :: [a] -> Int -> a -> [a]
replaceByIndex [] _ _ = []
replaceByIndex (x:xs) 0 a = a:xs
replaceByIndex (x:xs) n a = x : replaceByIndex xs (n-1) a


dropPiece :: Board -> (Int, Int) -> Maybe Piece -> Board
dropPiece b (i,j) piece = replaceByIndex b i newRow
    where
        newRow = dropInRow (b!!i) j piece
        dropInRow :: BoardRow -> Int -> Maybe Piece -> BoardRow
        dropInRow row colNum piece = replaceByIndex row colNum (BoardField piece)

{-
playInRow :: BoardRow -> Int -> Maybe Piece -> (Board, MoveLegality)
playInRow row colNum piece = ( replaceByIndex row colNum (BoardField piece), dropIsLegal (row!!colNum) )

playMove :: Board -> (Int, Int) -> Maybe Piece -> (Board, MoveLegality)
playMove b (i,j) piece = (replaceByIndex b i newRow, legal)
    where (newRow, legal) = playInRow (b!!i) j piece
-}

type FlattenedBoard = [(BoardField, Int, Int)]

addCoordsToRow :: BoardRow -> Int -> FlattenedBoard
addCoordsToRow row rowNum = zip3 row (replicate rowL rowNum) [0..rowL]
    where rowL = length row

flattenWithCoords :: Board -> FlattenedBoard
flattenWithCoords b = concat $ zipWith addCoordsToRow b [0..boardRows b]


-- play on the first free field from top left, if there is any
playArbitraryDrop :: Board -> Maybe Piece -> Board
playArbitraryDrop b piece = maybePlayDrop b freeSpace piece
    where freeSpace = getFreeSpace b

maybePlayDrop :: Board -> Maybe (Int, Int) -> Maybe Piece -> Board
maybePlayDrop b coords piece = maybe b (\c->dropPiece b c piece) coords 


--retire this in favor of isLegalMove and isLegalAnywhereMove
getFreeSpace :: Board -> Maybe (Int, Int)
getFreeSpace b = getFreeSpace' $ flattenWithCoords b
    where
        getFreeSpace' :: FlattenedBoard -> Maybe (Int, Int)
        getFreeSpace' [] = Nothing
        getFreeSpace' ((field, row, col):fields)
            | field == BoardField Nothing = Just (row, col)
            | otherwise = getFreeSpace' fields


-- players stop dropping stones when the board is filled
dropPhaseEndCheck :: Board -> Bool
dropPhaseEndCheck b = isNothing $ getFreeSpace b

-- have the computer take turns until the board is filled

changeTurn :: Piece -> Piece
changeTurn White = Black
changeTurn Black = White


--arguments: initial position -> first player to move -> final position
dropPhase :: Board -> Piece -> Board
dropPhase b playerTurn
    | not $ dropPhaseEndCheck b = dropPhase (playArbitraryDrop b (Just playerTurn)) (changeTurn playerTurn)
    | otherwise = b --return the board at the end of the drop phase


-- for a Shift move two coords are needed, start and destination
-- for all types Piece is the color of the player making the move (not the color of the removed piece!)
data Move = Drop Piece (Int, Int) | Remove Piece (Int, Int) | Shift Piece (Int, Int) (Int, Int)


dropHereIsLegal :: BoardField -> Bool
dropHereIsLegal (BoardField Nothing) = True
dropHereIsLegal _ = False

removeHereIsLegal :: BoardField -> Piece -> Bool
removeHereIsLegal (BoardField (Just White)) Black = True
removeHereIsLegal (BoardField (Just Black)) White = True
removeHereIsLegal _ _ = False

-- check if a move is legal
canPlayMoveHere :: Board -> Move -> Bool
canPlayMoveHere b ( Drop p (i,j) ) = dropHereIsLegal $ b!!i!!j
canPlayMoveHere b ( Remove p (i,j) ) = removeHereIsLegal (b!!i!!j) p
canPlayMoveHere b ( Shift p (i,j) (i2,j2) ) = False --TODO


playArbitraryRemove :: Board -> Piece -> Board
playArbitraryRemove b piece = maybePlayDrop b freeSpace (Just piece)
    where freeSpace = getFreeSpace b

-- -- 2nd phase: each player removes one of their opponent's stones
-- removePhase :: Board -> Piece -> Board
-- removePhase b playerTurn = playArbitraryRemove bAfterFirstRemove (changeTurn playerTurn) playArbitraryRemove
--     where bAfterFirstRemove = playArbitraryRemove b playerTurn playArbitraryRemove


--get user input ->MoveType

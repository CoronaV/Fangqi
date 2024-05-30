module Moves where
import Board
    ( Board, BoardRow, BoardField(..), Piece (..), GameState(..), Phase(..), FlattenedBoard, flattenWithCoords, isAnyCornerOfSquare )
import Data.Maybe (isNothing, fromMaybe)
import GHC.Utils.Misc (count)

type MoveLegality = Bool

-- for a Shift move two coords are needed, start and destination
-- for all types Piece is the color of the player making the move (not the color of the removed piece!)
data Move = Drop Piece (Int, Int) | Remove Piece (Int, Int) | Shift Piece (Int, Int) (Int, Int)
    deriving Show


replaceByIndex :: [a] -> Int -> a -> [a]
replaceByIndex [] _ _ = []
replaceByIndex (x:xs) 0 a = a:xs
replaceByIndex (x:xs) n a = x : replaceByIndex xs (n-1) a

changeSpace :: Board -> (Int, Int) -> Maybe Piece -> Board
changeSpace b (i,j) piece = replaceByIndex b i newRow
    where
        newRow = dropInRow (b!!i) j piece
        dropInRow :: BoardRow -> Int -> Maybe Piece -> BoardRow
        dropInRow row colNum piece = replaceByIndex row colNum (BoardField piece)

-- maybePlayDrop :: Board -> Maybe (Int, Int) -> Maybe Piece -> Board
-- maybePlayDrop b coords piece = maybe b (\c->changeSpace b c piece) coords



removeHereIsLegal :: BoardField -> Piece -> Bool
removeHereIsLegal (BoardField (Just White)) Black = True
removeHereIsLegal (BoardField (Just Black)) White = True
removeHereIsLegal _ _ = False

-- check if a move is legal. The information about piece/player is contained in Move
canPlayMoveHere :: Board -> Move -> Bool
canPlayMoveHere b ( Drop p (i,j) ) = dropHereIsLegal $ b!!i!!j
canPlayMoveHere b ( Remove p (i,j) ) = removeHereIsLegal (b!!i!!j) p
canPlayMoveHere b ( Shift p (i,j) (i2,j2) ) = False --TODO



dropHereIsLegal :: BoardField -> Bool
dropHereIsLegal (BoardField Nothing) = True
dropHereIsLegal _ = False



changeBoard :: Board -> Move -> Board
changeBoard b (Drop piece (i,j)) = changeSpace b (i,j) (Just piece)
changeBoard b (Remove piece (i,j)) = changeSpace b (i,j) Nothing
changeBoard b (Shift piece (i,j) (i2,j2)) = b -- TODO

changeTurn :: Piece -> Piece
changeTurn White = Black
changeTurn Black = White


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


-- keep the gamestate the same if illegal to reprompt the move if the player is human? write a message to output? TODO
-- for now skip checking if the move is in the correct phase or the correct player is playing
-- phase changes will be checked in main.hs
checkLegalAndResolve :: GameState -> Move -> GameState
checkLegalAndResolve (GameState b piece phase) move
    | canPlayMoveHere b move = GameState (changeBoard b move) (changeTurn piece) phase
    | otherwise = GameState b piece phase


-- if a 2x2 square is not formed this function will leave the GameState as it is
-- needs a player to choose a piece to capture
-- the Move is the move that causes the capture, not the capturing move!
-- the GameState is the state after the move??
checkExecuteCapture :: (Player p) => p -> GameState -> Move -> GameState
checkExecuteCapture p gs move
    | checkCapture gs move = gs --TODO! change the gamestate with executeCapture
    | otherwise = gs


--TODO
-- executeCapture ::  (Player p) => p -> GameState -> Move -> GameState
-- executeCapture p gs move = _ --makeMove p gs -- should a 

checkCapture :: GameState -> Move -> Bool
checkCapture gs (Remove _ _) = False --removes never form squares
checkCapture (GameState b _ _ ) (Drop piece (i,j)) = isAnyCornerOfSquare b piece (i,j) -- check the four squares around the dropped piece
checkCapture gs (Shift piece (i,j) (i2,j2)) = False -- TODO for the purposes of forming squares, a piece arriving on a square is the same as a drop...*
{- *except that a piece should not be able to form a square with its former position!
w.
ww
->
.w
ww

->False!

-}

-- Drop Piece (Int, Int) | Remove Piece (Int, Int) | Shift Piece (Int, Int) (Int, Int)
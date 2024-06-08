module Input where
import Moves (Move (..), Capture(..))
import Board (Piece(..), Phase (..), GameState (..))
import Data.Char (ord, digitToInt, isAlpha, isDigit)
import GHC.Data.Maybe (isNothing)
import Control.Applicative (Applicative(liftA2))
import Debug.Trace (trace)

--get string: parse to Move

-- needs the current game state info to construct a full move
strToMove :: String -> Piece -> Phase -> Maybe Move
strToMove s piece PhaseDrop = fmap (Drop piece) (strToCoords s)
strToMove s piece PhaseRemove = fmap (Remove piece) (strToCoords s)
--strToMove s piece PhaseShift = _

{-
input format: "a5" or "5a" both mean (0,4), rows = numbers (indexing from 1!), columns = letters
for simplicity white space will not be accepted in this function
-}

colNameToCoord :: Char -> Int
colNameToCoord c = ord c - ord 'a'

errorInput :: Maybe a
errorInput = trace "Bad format! Type in coords in the format 'c5' or '5c'." Nothing

strToCoords :: String -> Maybe (Int,Int)
strToCoords [a, b]
    | isDigit a && isAlpha b = Just (digitToInt a - 1, colNameToCoord b) -- "5a"
    | isDigit b && isAlpha a = Just (digitToInt b - 1, colNameToCoord a) -- "a5"
    | otherwise = errorInput
strToCoords _ = errorInput

-- do: repeatedly ask for moves until the user submits a valid (if not legal) move

-- the input system is context-aware, the human will just type in coords and the remaining
-- info about the move will be filled in by the system

--while Nothing: repeat
requestMoveUntilGot :: GameState -> IO Move
requestMoveUntilGot (GameState b piece phase) = do
    putStr "Make a move. "
    if phase == PhaseDrop then do
        putStr "Drop piece to: "
        coords <- fmap strToCoords getLine
        maybe (requestMoveUntilGot (GameState b piece phase)) (return . Drop piece) coords
    else if phase == PhaseRemove then do
        putStr "Remove opponent piece from: "
        coords <- fmap strToCoords getLine
        maybe (requestMoveUntilGot (GameState b piece phase)) (return . Remove piece) coords       
    else do
        putStr "Move piece from: "
        coordsFrom <- fmap strToCoords getLine
        putStr "Move piece to: "
        coordsTo <- fmap strToCoords getLine
        maybe (requestMoveUntilGot (GameState b piece phase)) return (liftA2 (Shift piece) coordsFrom coordsTo)


getCapture :: GameState -> IO Capture
getCapture (GameState b piece phase) = do
    putStrLn "A capture is possible! Choose a stone to capture:"
    coords <- fmap strToCoords getLine
    maybe (getCapture (GameState b piece phase)) (return . Capture piece) coords
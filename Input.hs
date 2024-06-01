module Input where
import Moves (Move (..))
import Board (Piece(..), Phase (..), GameState (..))
import Data.Maybe (fromMaybe)
import GHC.IO (evaluate)
import Data.Char (ord, digitToInt, isAlpha, isDigit)
import GHC.Data.Maybe (isNothing)
import Control.Applicative (Applicative(liftA2))

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

strToCoords :: String -> Maybe (Int,Int)
strToCoords [a, b]
    | isDigit a && isAlpha b = Just (digitToInt a - 1, colNameToCoord b) -- "5a"
    | isDigit b && isAlpha a = Just (digitToInt b - 1, colNameToCoord a) -- "a5"
    | otherwise = Nothing
strToCoords _ = Nothing

-- do: repeatedly ask for moves until the user submits a valid (if not legal) move

-- TODO:Fix this!! to return a move type and piece correctly or rename to getCoords?
-- the input system is context-aware, the human will just type in coords and the remaining
-- info about the move will be filled in by the system
-- getMove :: IO (Int, Int)
-- getMove = do
--     putStrLn "Make a move:"
--     move <- fmap strToCoords getLine -- fmap (\x -> strToMove x White PhaseDrop) getLine --fmap (fromMaybe move1 . (\x -> strToMove x White PhaseDrop)) getLine
--     maybe getMove return move --while Nothing: repeat
    -- if isNothing move
    --     then do
    --         putStrLn "Incorrect input, please type the move in this format: 'c5' or 'a2'. "
    --         getMove
    --     else
    --     return move

-- test: fmap show getMove


--while Nothing: repeat
getMoveFRFRNoCap :: GameState -> IO Move
getMoveFRFRNoCap (GameState b piece phase) = do
    putStrLn "Make a move:" --TODO cleverer prompts
    if phase == PhaseDrop then do
        coords <- fmap strToCoords getLine
        maybe (getMoveFRFRNoCap (GameState b piece phase)) (return . Drop piece) coords
    else if phase == PhaseRemove then do
        coords <- fmap strToCoords getLine
        maybe (getMoveFRFRNoCap (GameState b piece phase)) (return . Remove piece) coords       
    else do
        coordsFrom <- fmap strToCoords getLine
        coordsTo <- fmap strToCoords getLine
        maybe (getMoveFRFRNoCap (GameState b piece phase)) return (liftA2 (Shift piece) coordsFrom coordsTo)

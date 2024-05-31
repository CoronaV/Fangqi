module Input where
import Moves (Move (..))
import Board (Piece(..), Phase (..))
import Data.Maybe (fromMaybe)
import GHC.IO (evaluate)
import Data.Char (ord, digitToInt, isAlpha, isDigit)
import GHC.Data.Maybe (isNothing)


--get string: parse to Move

--make a human Player entity

move1 :: Move
move1 = Drop White (0,0)


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

--TODO:Fix this!! to return a move type and piece correctly
-- the input system is context-aware, the human will just type in coords and the remaining
-- info about the move will be filled in by the system
getMove :: IO (Int, Int)
getMove = do
    putStrLn "Make a move:"
    move <- fmap strToCoords getLine -- fmap (\x -> strToMove x White PhaseDrop) getLine --fmap (fromMaybe move1 . (\x -> strToMove x White PhaseDrop)) getLine
    maybe getMove return move --while Nothing: repeat

    --return move1

    -- if isNothing move
    --     then do
    --         putStrLn "Incorrect input, please type the move in this format: 'c5' or 'a2'. "
    --         getMove
    --     else
    --     return move

-- test: fmap show getMove

import Moves (Player(..), Move (..))
import Board (Piece(..), Phase (..), Board, boardCols, boardRows)
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
input format: "a5" or "5a" both mean (0,5), rows = numbers, columns = letters
for simplicity white space will not be accepted in this function
-}

colNameToCoord :: Char -> Int
colNameToCoord c = ord c - ord 'a'

strToCoords :: String -> Maybe (Int,Int)
strToCoords [a, b]
    | isDigit a && isAlpha b = Just (colNameToCoord b, digitToInt a) -- "5a"
    | isDigit b && isAlpha a = Just (colNameToCoord a, digitToInt b) -- "a5"
    | otherwise = Nothing
strToCoords _ = Nothing

coordInBounds :: Board -> (Int,Int) -> Bool
coordInBounds b (i,j) = i >= 0 && j >= 0 && i < boardRows b && j < boardCols b


-- do: repeatedly ask for moves until the user submits a valid (if not legal) move
getMove :: IO Move
getMove = do
    putStrLn "Make a move:"
    move <- fmap (\x -> strToMove x White PhaseDrop) getLine --fmap (fromMaybe move1 . (\x -> strToMove x White PhaseDrop)) getLine
    fmap (\x -> fromMaybe x move) getMove

    -- if isNothing move
    --     then do
    --         putStrLn "Incorrect input, please type the move in this format: 'c5' or 'a2'. "
    --         getMove
    --     else
    --     return move

-- test: fmap show getMove

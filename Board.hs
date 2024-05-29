module Board where

{- 

Xinjiang Fāngqí is played on a 7×7 board.
-}


data Piece = Black | White
    deriving Eq
-- instance Show Piece where
--     show :: Piece -> String
--     show Black = "b"
--     show White = "w"

-- "type" instead?
data BoardField = BoardField (Maybe Piece)
    deriving Eq
instance Show BoardField where
    show :: BoardField -> String
    show (BoardField Nothing) = "."
    show (BoardField ( Just Black)) = "b"
    show (BoardField ( Just White)) = "w"

type BoardRow = [BoardField]

-- the row index comes first!
type Board = [BoardRow]

boardRows :: Board -> Int
boardRows = length

boardCols :: Board -> Int
boardCols (r:rs) = length r

printBoardRow :: BoardRow -> String
printBoardRow br = unwords (map show br)

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (zipWith (\x y -> [x,y]) xs ys)


letters :: Int -> [Char]
letters n = take n ['a'..]

printBoardBottomEdge :: Board -> String
printBoardBottomEdge b = " " ++ concat (replicate (2*l) "-") ++ "\n  " ++ interleave (letters l) (replicate l ' ')
    where l = length b

printBoard :: Board -> String
printBoard b = unlines $ zipWith addBoardLeftEdge (map printBoardRow b) [1..]

addBoardLeftEdge :: String -> Int -> String
addBoardLeftEdge str rowNum = show rowNum ++ "|" ++ str

-- putStrLn is necessary to correctly display newlines (instead of show)
displayBoard :: Board -> IO()
displayBoard b = do
        putStrLn $ printBoard b ++ printBoardBottomEdge b


emptyField :: BoardField
emptyField = BoardField Nothing

emptyRow :: Int -> BoardRow
emptyRow size = replicate size emptyField

emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = replicate rows (emptyRow cols)

data Phase = PhaseDrop | PhaseRemove | PhaseShift
-- board, whose move it is + what is the current phase?
data GameState = GameState Board Piece Phase
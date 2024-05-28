
{- 

Xinjiang Fāngqí is played on a 7×7 board.
-}


data Piece = Black | White
-- instance Show Piece where
--     show :: Piece -> String
--     show Black = "b"
--     show White = "w"

-- "type" instead?
data BoardField = BoardField (Maybe Piece)
instance Show BoardField where
    show (BoardField Nothing) = "."
    show (BoardField ( Just Black)) = "b"
    show (BoardField ( Just White)) = "w"

type BoardRow = [BoardField]

type Board = [BoardRow]

printBoardRow :: BoardRow -> String
printBoardRow br = unwords (map show br)

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (zipWith (\x y -> [x,y]) xs ys)


letters :: Int -> [Char]
letters n = take n ['a'..]

printBoardBottomEdge :: Board -> String
printBoardBottomEdge b = concat (replicate (2*l) "-") ++ "\n" ++ interleave (letters l) (replicate l ' ')
    where l = length b


printBoard :: Board -> String
printBoard b = unlines (map printBoardRow b)

-- putStrLn is necessary to correctly display newlines (instead of show)
displayBoard :: Board -> IO()
displayBoard b = do
        putStrLn $ printBoard b
        putStrLn $ printBoardBottomEdge b

emptyField :: BoardField
emptyField = BoardField Nothing

emptyRow :: Int -> BoardRow
emptyRow size = replicate size emptyField

emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = replicate rows (emptyRow cols)


chessBoard = emptyBoard 8 8

--board toString function - haskell idiom?

printBoardn = putStrLn "kok"


module Board where

{- 

Xinjiang Fāngqí is played on a 7×7 board.
-}


data Piece = Black | White
    deriving (Eq, Show)
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
    deriving Show
-- board, whose move it is + what is the current phase?
data GameState = GameState Board Piece Phase
    deriving Show


spaceHasType :: Board -> (Int, Int) -> BoardField -> Bool
spaceHasType b (i,j) fieldType = b!!i!!j == fieldType

-- necessary for checking 2x2 square formations for captures:
isLeftUpCornerOfSquare :: Board -> Piece -> (Int, Int) -> Bool
-- if the intersection is on the bottom or right of the board -> never is upper left corner of 2x2 square
isLeftUpCornerOfSquare b piece (i,j) = boardRows b > i && boardCols b > j && checkSquareStones b piece (i,j)
    where
        checkSquareStones b piece (i,j) = spaceHasType b (i,j) (BoardField $ Just piece) &&
                                          spaceHasType b (i+1,j) (BoardField $ Just piece) &&
                                          spaceHasType b (i,j+1) (BoardField $ Just piece) &&
                                          spaceHasType b (i+1,j+1) (BoardField $ Just piece)


-- for checking if a dropped piece forms a square
-- if multiple squares are formed at once, let's implement the variant that removes just one piece?
isAnyCornerOfSquare :: Board -> Piece -> (Int, Int) -> Bool
isAnyCornerOfSquare b piece (i,j) = isLeftUpCornerOfSquare b piece (i,j) ||
                                    isLeftUpCornerOfSquare b piece (i-1,j) ||
                                    isLeftUpCornerOfSquare b piece (i,j-1) ||
                                    isLeftUpCornerOfSquare b piece (i-1,j-1)


type FlattenedBoard = [(BoardField, Int, Int)]

addCoordsToRow :: BoardRow -> Int -> FlattenedBoard
addCoordsToRow row rowNum = zip3 row (replicate rowL rowNum) [0..rowL]
    where rowL = length row

flattenWithCoords :: Board -> FlattenedBoard
flattenWithCoords b = concat $ zipWith addCoordsToRow b [0..boardRows b]
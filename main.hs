
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


chessBoard = emptyBoard 8 8


-- functions related to making moves
-- there are 2 types of moves: placing a stone and moving a stone to a neighboring intersection (=field,square)

-- a placing move is legal if the intersection was empty before the move
type MoveLegality = Bool

replaceByIndex :: [a] -> Int -> a -> [a]
replaceByIndex [] _ _ = []
replaceByIndex (x:xs) 0 a = a:xs
replaceByIndex (x:xs) n a = x : replaceByIndex xs (n-1) a

dropIsLegal :: Board -> (Int, Int) -> MoveLegality
dropIsLegal b (i,j) = dropHereIsLegal (b!!i!!j)

dropHereIsLegal :: BoardField -> Bool
dropHereIsLegal (BoardField Nothing) = True
dropHereIsLegal _ = False


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
playArbitraryMove :: Board -> Maybe Piece -> Board
playArbitraryMove b piece = maybePlayMove b freeSpace piece
    where freeSpace = getFreeSpace (flattenWithCoords b)

maybePlayMove :: Board -> Maybe (Int, Int) -> Maybe Piece -> Board
maybePlayMove b coords piece = maybe b (\c->dropPiece b c piece) coords 

getFreeSpace :: FlattenedBoard -> Maybe (Int, Int)
getFreeSpace [] = Nothing
getFreeSpace ((field, row, col):fields)
    | field == BoardField Nothing = Just (row, col)
    | otherwise = getFreeSpace fields



mainLoop :: IO()
mainLoop = _



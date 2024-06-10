module Board where


--Xinjiang Fāngqí is played on a 7×7 [square tiling] board.

data Piece = Black | White
    deriving (Eq, Show)

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
    deriving (Show, Eq)
-- board, whose move it is + what is the current phase?
data GameState = GameState { board :: Board, piece :: Piece, phase :: Phase } --Board Piece Phase
    deriving Show

unpackGSToBoard :: (Board -> a) -> GameState -> a
unpackGSToBoard boardFunc (GameState b p phase) = boardFunc b

unpackGSToPhase :: (Phase -> a) -> GameState -> a
unpackGSToPhase phaseFunc (GameState b p phase) = phaseFunc phase

displayGameState :: GameState -> IO()
displayGameState (GameState b piece phase) = do
        displayBoard b
        putStrLn $ "It is now "++ show piece ++ "'s turn in the phase "++ show phase


spaceHasType :: Board -> (Int, Int) -> BoardField -> Bool
spaceHasType b (i,j) fieldType = b!!i!!j == fieldType


isInBounds :: Board -> (Int, Int) -> Bool
isInBounds b (i,j) = i >= 0 && j >= 0 && boardRows b > i && boardCols b > j

-- a function that reports back "empty spaces" beyond the edge of board, to avoid exceptions
-- and unwieldy conditionals
spaceHasTypeEmptyExtend ::  Board -> (Int, Int) -> BoardField -> Bool
spaceHasTypeEmptyExtend b (i,j) fieldType
    | isInBounds b (i,j) = spaceHasType b (i,j) fieldType
    | otherwise = fieldType == BoardField Nothing

-- necessary for checking 2x2 square formations for captures:
isLeftUpCornerOfSquare :: Board -> Piece -> (Int, Int) -> Bool
-- if the intersection is on the bottom or right of the board -> never is upper left corner of 2x2 square
isLeftUpCornerOfSquare b piece (i,j) = checkSquareStones b piece (i,j)
    where
        checkSquareStones b piece (i,j) = spaceHasTypeEmptyExtend b (i,j) (BoardField $ Just piece) &&
                                          spaceHasTypeEmptyExtend b (i+1,j) (BoardField $ Just piece) &&
                                          spaceHasTypeEmptyExtend b (i,j+1) (BoardField $ Just piece) &&
                                          spaceHasTypeEmptyExtend b (i+1,j+1) (BoardField $ Just piece)


-- for checking if a dropped piece forms a square
-- if multiple squares are formed at once, let's implement the variant that removes just one piece?(
-- problem: if we receive e.g. (0,0), we should be checking only one square or we will get negative indices!
-- solution: use a function that reports back "empty spaces" beyond the edge of board -> spaceHasTypeEmptyExtend
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

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

getAllCoords :: Board -> [(Int, Int)]
getAllCoords b = cartesianProduct [0..boardRows b-1] [0..boardCols b-1]

-- useful for heuristics
-- typically places adjacent to both your pieces and the opponent's pieces are good to get your pieces into
-- or remove opponent's pieces from
-- counts orthogonal neigbors only for simplicity
getNonEmptyNeighborCount :: Board -> (Int, Int) -> Int
getNonEmptyNeighborCount b (i,j) = numberOfPiecesOnSquare b (i+1,j) +
                                    numberOfPiecesOnSquare b (i,j+1) +
                                    numberOfPiecesOnSquare b (i-1,j) +
                                    numberOfPiecesOnSquare b (i,j-1)

numberOfPiecesOnSquare :: Board -> (Int, Int) -> Int
numberOfPiecesOnSquare b (i,j)
            | spaceHasTypeEmptyExtend b (i,j) (BoardField Nothing) = 0
            | otherwise = 1
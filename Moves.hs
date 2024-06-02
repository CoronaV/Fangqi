module Moves where
import Board
    ( Board, BoardRow, BoardField(..), Piece (..), GameState(..), Phase(..), FlattenedBoard, flattenWithCoords, isAnyCornerOfSquare, boardRows, boardCols, isInBounds, emptyBoard, emptyRow, spaceHasTypeEmptyExtend )
import Data.Maybe (isNothing, fromMaybe)
import GHC.Utils.Misc (count)


-- functions related to making moves
-- there are 3 types of moves: placing a stone, capturing a stone, and moving a stone to a neighboring intersection (=field,square)

-- a placing move is legal if the intersection was empty before the move

-- for a Shift move two coords are needed, start and destination
-- for all types Piece is the color of the player making the move (not the color of the removed piece!)
data Move = Drop Piece (Int, Int) | Remove Piece (Int, Int) | Shift Piece (Int, Int) (Int, Int)
    deriving Show


data Capture = Capture Piece (Int, Int)
    deriving Show

-- TODO: actually, shouldn't a Move contain the following capture if it occurs??
-- that would make the minimax simpler

-- a move with *potentially* a capture included
-- a capture move can occur independently, however, in the second phase!
data MoveCapture = MoveWithCapture Move Capture | MoveWithoutCapture Move
    --MoveWithCapture Move (Maybe Move )
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

removeHereIsLegal :: BoardField -> Piece -> Bool
removeHereIsLegal (BoardField (Just White)) Black = True
removeHereIsLegal (BoardField (Just Black)) White = True
removeHereIsLegal _ _ = False

shiftIsLegal :: Board -> Move -> Bool
shiftIsLegal b ( Shift p (i,j) (i2,j2) ) = spaceHasTypeEmptyExtend b (i,j) (BoardField (Just p)) &&
                                            elem (i2,j2) ( pieceCanMoveTo b (i,j) )

-- check if a move is legal. The information about piece/player is contained in Move
canPlayMoveHere :: Board -> Move -> Bool
canPlayMoveHere b ( Drop p (i,j) ) = dropHereIsLegal $ b!!i!!j
canPlayMoveHere b ( Remove p (i,j) ) = removeHereIsLegal (b!!i!!j) p
canPlayMoveHere b ( Shift p (i,j) (i2,j2) ) = shiftIsLegal b ( Shift p (i,j) (i2,j2) )

canPlayCaptureHere :: Board -> Capture -> Bool
canPlayCaptureHere b ( Capture p (i,j) ) = removeHereIsLegal (b!!i!!j) p

dropHereIsLegal :: BoardField -> Bool
dropHereIsLegal (BoardField Nothing) = True
dropHereIsLegal _ = False

changeBoard :: Board -> Move -> Board
changeBoard b (Drop piece (i,j)) = changeSpace b (i,j) (Just piece)
changeBoard b (Remove piece (i,j)) = changeSpace b (i,j) Nothing
changeBoard b (Shift piece (i,j) (i2,j2)) = changeSpace (changeSpace b (i,j) Nothing) (i2,j2) (Just piece)

changeBoardMC :: Board -> MoveCapture -> Board
changeBoardMC b (MoveWithCapture move (Capture piece (i,j))) = changeSpace (changeBoard b move) (i,j) Nothing
changeBoardMC b (MoveWithoutCapture move) = changeBoard b move

switchColor :: Piece -> Piece
switchColor White = Black
switchColor Black = White


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
getSpaceTypeNumber b fieldType = count (fieldType ==) (concat b)


-- keep the gamestate the same if illegal to reprompt the move if the player is human? write a message to output? TODO
-- for now skip checking if the move is in the correct phase or the correct player is playing
-- phase changes will be checked in main.hs


moveInBounds :: Board -> Move -> Bool
moveInBounds b (Drop piece (i,j)) = isInBounds b (i,j)
moveInBounds b (Remove piece (i,j)) = isInBounds b (i,j)
moveInBounds b (Shift piece (i,j) (i2,j2)) = isInBounds b (i,j) && isInBounds b (i2,j2)

captureInBounds :: Board -> Capture -> Bool
captureInBounds b (Capture piece (i,j)) = isInBounds b (i,j)


-- no need to check if the move is of the correct type, the game will just not allow wrong move types
-- should we check just in case if the gamestate color to play corresponds to the move color?
isLegal :: GameState -> Move -> Bool
isLegal (GameState b piece _) move = moveInBounds b move && canPlayMoveHere b move

isLegalCapture :: GameState -> Capture -> Bool
isLegalCapture (GameState b piece _) capture = captureInBounds b capture && canPlayCaptureHere b capture

-- don't need to change the board between checking the move and capture because you only capture enemy pieces,
-- which stay in place during a drop/shift move, and you can't trigger a capture on a remove move
isLegalMC :: GameState -> MoveCapture -> Bool
isLegalMC gs (MoveWithCapture move capture) = isLegal gs move && isLegalCapture gs capture
isLegalMC gs (MoveWithoutCapture move) = isLegal gs move

-- check if move is in bounds and is played on a valid target field
-- will not change turn because it's necessary to capture with the same color if a capture is possible before changing turn
-- and the capture can't be included here to keep the IO monad out of this module
-- TODO: check in a 'shift' move if the coords are in a row or column and spaces between them are free 
checkLegalAndResolve :: GameState -> Move -> GameState
checkLegalAndResolve (GameState b piece phase) move
    | isLegal (GameState b piece phase) move = GameState (changeBoard b move) piece phase
    | otherwise = GameState b piece phase

checkLegalAndResolveMC :: GameState -> MoveCapture -> GameState
checkLegalAndResolveMC (GameState b piece phase) move
    | isLegalMC (GameState b piece phase) move = GameState (changeBoardMC b move) piece phase
    | otherwise = GameState b piece phase

-- the last move is a necessary argument to check which squares have been newly formed
-- the gamestate argument will be the gamestate *after* the move happened to avoid simulating the move anyways
-- only to discard the results and execute the move later
-- resolveCapture :: GameState -> Move -> GameState
-- resolveCapture gs move
--     | checkCapture = 
--     | otherwise = gs -- no captures, keep the same state


--  reports back Bool capture has happened Y/N to the main loop
-- which will choose the capturing move and send it here to be executed
-- note: a piece should not be able to form a square with its former position!
-- to this end, captures will be checked *after* the move is executed
checkCapture :: GameState -> Move -> Bool
checkCapture gs (Remove _ _) = False --removes never form squares
checkCapture (GameState b _ _ ) (Drop piece (i,j)) = isAnyCornerOfSquare b piece (i,j) -- check the four squares around the dropped piece
checkCapture gs (Shift piece (i,j) (i2,j2)) = False -- TODO for the purposes of forming squares, a piece arriving on a square is the same as a drop...*


--TODO: merge this (BoardField, Int, Int) -> GameState -> Move with functions handling 
-- player input?
coordsToMove :: GameState -> (BoardField, Int, Int) -> Move
coordsToMove (GameState b piece PhaseDrop)  (_, i, j) = Drop piece (i,j)
coordsToMove (GameState b piece PhaseRemove)  (_, i, j) = Remove piece (i,j)


coordsFromToShiftMoveList :: GameState -> (Int, Int) -> [Move]
coordsFromToShiftMoveList (GameState b piece PhaseShift) (fromI, fromJ) = map (Shift piece (fromI,fromJ)) destinations
    where
        destinations = pieceCanMoveTo b (fromI, fromJ)
        --Shift piece fromI fromJ

-- lists all spaces a piece can move to with a Shift move from a specified square
-- take four vectors, takeWhile empty spaces in the direction of each
-- this function does not check if there is a piece at the "from" square!
pieceCanMoveTo :: Board -> (Int, Int) -> [(Int, Int)]
pieceCanMoveTo b from = freeSpacesInDirection b from (0,1) ++
                        freeSpacesInDirection b from (0,-1) ++
                        freeSpacesInDirection b from (1,0) ++
                        freeSpacesInDirection b from (-1,0)

-- TODO: move to Board.hs
-- only the four orthogonal unit directions are necessary, thankfully
freeSpacesInDirection :: Board -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
freeSpacesInDirection b (fromI, fromJ) (vectI,vectJ)
    --check if the neighboring space is free; otherwise no fields at all can be reached in this direction
    -- also check if it is in bounds!
    | isInBounds b (newI,newJ) && b!!newI!!newJ == BoardField Nothing = (newI, newJ) : freeSpacesInDirection b (newI, newJ) (vectI,vectJ)
    | otherwise = []
    where
        newI = fromI+vectI
        newJ = fromJ+vectJ

oneStoneBoard :: Board
oneStoneBoard = [emptyRow 3, [BoardField Nothing, BoardField (Just Black), BoardField Nothing], emptyRow 3]
testPossMoves :: [Move]
testPossMoves = getPossibleMoves (GameState oneStoneBoard Black PhaseShift)

getPossibleMoves :: GameState -> [Move]
getPossibleMoves (GameState b piece PhaseDrop) = map (coordsToMove (GameState b piece PhaseDrop)) (filter isEmpty (flattenWithCoords b))
    where
        isEmpty :: (BoardField, Int, Int) -> Bool
        isEmpty (bf, _, _) = bf == BoardField Nothing
getPossibleMoves (GameState b piece PhaseRemove) = map (coordsToMove (GameState b piece PhaseRemove)) (filter isOpponent (flattenWithCoords b))
    where
        isOpponent :: (BoardField, Int, Int) -> Bool
        isOpponent (bf, _, _) = bf == BoardField (Just $ switchColor piece)

-- legal shift move: 
-- from coord is own stone

getPossibleMoves (GameState b piece PhaseShift) = concatMap (coordsFromToShiftMoveList (GameState b piece PhaseShift)) fromSquares
    where
        fromSquares = map toCoords $ filter isOwn (flattenWithCoords b)
        isOwn :: (BoardField, Int, Int) -> Bool
        isOwn (bf, _, _) = bf == BoardField (Just piece)
        toCoords :: (BoardField, Int, Int) -> (Int, Int)
        toCoords (_, i, j) = (i,j)


coordsToCapture :: GameState -> (BoardField, Int, Int) -> Capture
coordsToCapture (GameState b piece _)  (_, i, j) = Capture piece (i,j)

getPossibleCaptures :: GameState -> [Capture]
getPossibleCaptures (GameState b piece phase) = map (coordsToCapture (GameState b piece phase)) (filter isEmpty (flattenWithCoords b))
    where
        isEmpty :: (BoardField, Int, Int) -> Bool
        isEmpty (bf, _, _) = bf == BoardField Nothing


getCapturesIfApplicable :: GameState -> Move -> [MoveCapture]
getCapturesIfApplicable gs move
    | checkCapture gs move = map (MoveWithCapture move) (getPossibleCaptures gs) -- a capturing move
    | otherwise = [MoveWithoutCapture move]

getPossibleMCs :: GameState -> [MoveCapture]
getPossibleMCs gs = concatMap (getCapturesIfApplicable gs) (getPossibleMoves gs)
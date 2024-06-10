module Player where
import Board (GameState (..), Phase (..), BoardField (..), Piece (..), Board, getNonEmptyNeighborCount)
import Moves (Move (..), Capture(..), isLegal, getSpaceTypeNumber, getPossibleMoves, checkLegalAndResolve, MoveCapture (..), getPossibleCaptures, isLegalCapture, isLegalMC, getPossibleMCs, checkLegalAndResolveMC, checkCaptureAfter, checkCaptureBefore, describeMoveCapture)
import Input (requestMoveUntilGot, getCapture)
import Control.Applicative (Applicative(liftA2))
import GHC.Float (int2Float)
import Data.List ( sortBy )

-- "player" is an interface with a method for choosing moves
-- it returns IO Move - i.e. there is no guarantee that the same player will choose the same move twice
-- (necessary primarily to implement human players)
class Player p where
    chooseMoveCapture :: p -> GameState -> IO MoveCapture

data RandomAI = RandomAI

instance Player RandomAI where
    chooseMoveCapture :: RandomAI -> GameState -> IO MoveCapture
    chooseMoveCapture RandomAI gs = do
        let chosenMove = head (getPossibleMoves gs)
        let newState = checkLegalAndResolve gs chosenMove
        let mayCapture = checkCaptureAfter newState chosenMove
        if mayCapture then do
            chosenCapture <- chooseCapture RandomAI gs
            return $ MoveWithCapture chosenMove chosenCapture
        else do -- no capture
            return $ MoveWithoutCapture chosenMove
        where
            chooseMove :: RandomAI -> GameState -> IO Move
            -- if there is no legal move, a function checking for end states messed up, so the AI will just make whatever move e.g. (0,0)
            -- and cause an exception down the line
            -- play on the first free field from top left, if there is any
            chooseMove RandomAI gs = return $ head (getPossibleMoves gs)
            chooseCapture :: RandomAI -> GameState -> IO Capture
            chooseCapture RandomAI gs = return $ head (getPossibleCaptures gs)

data Human = Human

instance Player Human where
    chooseMoveCapture :: Human -> GameState -> IO MoveCapture
    chooseMoveCapture Human gs = do
        chosenMove <- chooseMove Human gs
        let newState = checkLegalAndResolve gs chosenMove
        let mayCapture = checkCaptureAfter newState chosenMove
        if mayCapture then do
            chosenCapture <- chooseCapture Human gs
            return $ MoveWithCapture chosenMove chosenCapture
        else do -- no capture
            return $ MoveWithoutCapture chosenMove
        where
            chooseMove :: Human -> GameState -> IO Move
            -- the input system is context-aware, the human will just type in coords and the remaining
            -- info about the move will be filled in by the system
            chooseMove Human gs = do
                -- "while move is illegal: get move"
                move <- requestMoveUntilGot gs
                if isLegal gs move
                    then do
                        return move
                    else do
                        putStrLn "Illegal move! Choose another one."
                        chooseMove Human gs
            chooseCapture :: Human -> GameState -> IO Capture
            chooseCapture Human gs = do
                -- "while move is illegal: get move"
                capture <- getCapture gs
                if isLegalCapture gs capture
                    then do
                        return capture
                    else do
                        chooseCapture Human gs

-- put this inside Player/chooseMove instead?

-- if a 2x2 square is not formed this function will leave the GameState as it is
-- needs a player to choose a piece to capture
-- the Move is the move that causes the capture, not the capturing move!
-- the GameState is the state after the move??
checkExecuteCapture :: (Player p) => p -> GameState -> Move -> GameState
checkExecuteCapture p gs move
    | checkCaptureAfter gs move = gs --TODO! change the gamestate with executeCapture
    | otherwise = gs

-- the heuristic will say how good a position is for White

-- goodness = # white stones / # black stones
-- not a subtraction because having 1 stone advantage is more important if there are fewer stones
-- ...at least in the "shift phase" 

-- TODO: Win states should have + or - infinity

-- should work fine (returning Infinity) even if black stones are 0
heuristic :: GameState -> Float
heuristic (GameState b _ _ ) = getPlayerStonesFloat b White / getPlayerStonesFloat b Black
    where
        getPlayerStonesFloat :: Board -> Piece -> Float
        getPlayerStonesFloat b playerColor = int2Float $ getSpaceTypeNumber b (BoardField $ Just playerColor)

-- the MoveCapture containing the following capture if it occurs makes the minimax simpler
    -- problem: we need to simulate all possible captures as well
    -- and some moves will induce captures while others won't
    -- solution: replace child states that result in a capture with a set of child states after all possible captures
    -- though this will increase the branching factor considerably...

-- bool argument: reverse/get minimum instead
getIndexOfMaximum :: Ord a => [a] -> Bool -> Int
getIndexOfMaximum xs True = head $ filter ((== maximum xs) . (xs !!)) [0..]
getIndexOfMaximum xs False = head $ filter ((== minimum xs) . (xs !!)) [0..]

-- int argument: depth of evaluation
-- returns best move and value of best move for recursion
-- essentially this function should just simulate "play", except that it cuts off at a certain depth and returns an evaluation...

minimaxMoveGetter :: Int -> GameState -> (MoveCapture, Float)
minimaxMoveGetter depth gs = do
    let moveCaps = getBestHeuristicMCs gs -- get available combinations of move (+ capture, if applicable)
    -- do something if zero moves are available! (i.e. the game has ended... or the phase has ended)
    -- or, ideally, rewrite this so it fully simulates the Play function
    --actually, instead check if the phase has ended
    
    if null moveCaps then do
        (dummyMove, 1)
    else do
        minimaxGetBestMove depth gs moveCaps
    where dummyMove = MoveWithoutCapture (Drop White (0,0))

minimaxGetBestMove :: Int -> GameState -> [MoveCapture] -> (MoveCapture, Float)
minimaxGetBestMove 0 (GameState b playerColor phase) moveCaps = do -- end recursion at depth 0
    let childStates = map (checkLegalAndResolveMC gs) moveCaps -- the legality check should be unnecessary, moves should contain only legal moves
    -- evaluate child states and return the move that leads to the best one
    let evaluations = map heuristic childStates
    let bestChildIndex = getIndexOfMaximum evaluations (playerColor == White) -- if White, want to maximize the heuristic value
    (moveCaps!!bestChildIndex, evaluations!!bestChildIndex)

    where gs = GameState b playerColor phase

minimaxGetBestMove depth (GameState b playerColor phase) moveCaps = do
    let childStates = map (checkLegalAndResolveMC gs) moveCaps -- the legality check should be unnecessary, moves should contain only legal moves
    -- what exactly are the best responses to the investigated moves doesn't interest us, we want just the evaluation of the moves
    let (_,evaluations) = unzip $ map (minimaxMoveGetter (depth-1)) childStates
    let bestChildIndex = getIndexOfMaximum evaluations (playerColor == White) -- if White, want to maximize the heuristic value
    (moveCaps!!bestChildIndex, evaluations!!bestChildIndex)

    where gs = GameState b playerColor phase

data HeuristicAI = HeuristicAI

instance Player HeuristicAI where
    chooseMoveCapture :: HeuristicAI -> GameState -> IO MoveCapture
    chooseMoveCapture HeuristicAI gs = do
        let chosenMove = fst (minimaxMoveGetter 3 gs)
        putStrLn $ "The AI has chosen to: " ++ describeMoveCapture chosenMove
        return chosenMove

-- piece is the *capturing* color
getCaptureHeuristic :: Board -> Capture -> Int
getCaptureHeuristic b (Capture piece (i,j)) = getNonEmptyNeighborCount b (i,j)

compareHeuristics :: (a, Int) -> (a, Int) -> Ordering
compareHeuristics a b = if snd a < snd b then GT else LT -- "reversed" to make the sorts below put the highest heuristics at the beginning

-- often it doesn't make much of a difference which pieces you capture
-- typically it's better to capture enemy pieces that are next to more enemy pieces (forming a square)
-- to improve performance on a large board choose N captures and apply them
getNBestCapturesHeuristic :: Int -> GameState -> Move -> [MoveCapture]
getNBestCapturesHeuristic n (GameState board piece phase) move
    | checkCaptureBefore gs move = do
        let captures = getPossibleCaptures gs -- a capturing move
        let heuristicValues = map (getCaptureHeuristic board) captures
        let zipped = zip captures heuristicValues
        let sorted = sortBy compareHeuristics zipped
        let bestCaptures = take n $ map fst sorted --extract the best captures
        map (MoveWithCapture move) bestCaptures
    | otherwise = [MoveWithoutCapture move]
    where gs = GameState board piece phase

-- it's good to drop in the middle of own and enemy pieces
getMoveHeuristic :: Board -> Move -> Int
getMoveHeuristic b (Drop piece (i,j)) = getNonEmptyNeighborCount b (i,j)
getMoveHeuristic b (Remove piece (i,j)) = getNonEmptyNeighborCount b (i,j)
getMoveHeuristic b (Shift piece (i,j) (i2,j2)) = getNonEmptyNeighborCount b (i2,j2) -- it is important to move *toward* groups of pieces

getNBestMovesHeuristic :: Int -> GameState -> [Move]
getNBestMovesHeuristic n (GameState board piece phase) = do
    let moves = getPossibleMoves gs
    let heuristicValues = map (getMoveHeuristic board) moves
    let zipped = zip moves heuristicValues
    let sorted = sortBy compareHeuristics zipped
    let bestMoves = take n $ map fst sorted --extract the best captures
    bestMoves
    where gs = GameState board piece phase

getBestHeuristicMCs :: GameState -> [MoveCapture]
getBestHeuristicMCs gs = concatMap (getNBestCapturesHeuristic 5 gs) (getNBestMovesHeuristic 14 gs)


-- playPhaseMinimax :: Int -> (GameState -> Bool) -> GameState -> (IO GameState, Float)
-- playPhaseMinimax depth endCondition current = do
--     let ended = endCondition current
--     if ended
--         then return current
--     else do
--         chosenMoveCapture <- chooseMoveCapture HeuristicAI current
--         let afterMoveCapture = checkLegalAndResolveMC current chosenMoveCapture
--         displayGameState afterMoveCapture
--         playPhaseMinimax HeuristicAI HeuristicAI endCondition afterMoveCapture
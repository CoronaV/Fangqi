module Player where
import Board (GameState (..), Phase (..), BoardField (..), Piece (..), Board)
import Moves (Move (..), Capture(..), getSpaceOfType, switchColor, checkCapture, isLegal, getSpaceTypeNumber, getPossibleMoves, checkLegalAndResolve, MoveCapture (..), getPossibleCaptures, isLegalCapture, isLegalMC)
import Input (getMoveFRFRNoCap, getCapture)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative(liftA2))
import GHC.Float (int2Float)
import Data.List ( partition )
import GHC.Utils.Misc (partitionByList)

-- "player" is an interface with a method for choosing moves
-- it returns IO Move - i.e. there is no guarantee that the same player will choose the same move twice
-- (necessary primarily to implement human players) 
class Player p where
    chooseMove :: p -> GameState -> IO Move -- a "regular move"
    chooseCapture :: p -> GameState -> IO Capture --rename this 
    chooseMoveCapture :: p -> GameState -> IO MoveCapture


-- refactor this to use getPossibleMoves?
-- also put a check for getPossibleMoves returning 0 moves in the main loop -> end game
data RandomAI = RandomAI

instance Player RandomAI where
    chooseMove :: RandomAI -> GameState -> IO Move
    -- if there is no legal move, we messed up, so the AI will just make whatever move e.g. (0,0)
    -- and cause an exception down the line
    -- play on the first free field from top left, if there is any
    chooseMove RandomAI gs = return $ head (getPossibleMoves gs)
    -- chooseMove RandomAI (GameState b piece PhaseDrop) = return $ Drop piece $ fromMaybe (0,0) (getSpaceOfType b (BoardField Nothing) )
    -- chooseMove RandomAI (GameState b piece PhaseRemove) = return $ Remove piece $ fromMaybe (0,0) (getSpaceOfType b (BoardField (Just $ switchColor piece)) )
    -- chooseMove RandomAI (GameState b piece PhaseShift) = return $ Shift piece (0,0) (0,0) --TODO: get one of the player's stones that has a neighboring space free, move it there
    chooseCapture :: RandomAI -> GameState -> IO Capture
    chooseCapture RandomAI gs = return $ head (getPossibleCaptures gs)

    chooseMoveCapture :: RandomAI -> GameState -> IO MoveCapture
    chooseMoveCapture RandomAI gs = do
        let chosenMove = head (getPossibleMoves gs)
        let newState = checkLegalAndResolve gs chosenMove
        let mayCapture = checkCapture newState chosenMove
        if mayCapture then do
            chosenCapture <- chooseCapture RandomAI gs
            return $ MoveWithCapture chosenMove chosenCapture
        else do -- no capture
            return $ MoveWithoutCapture chosenMove

data Human = Human --merge with RandomAI into one type with two constructors?

--TODO: need to check human moves (incl. captures) for legality..

-- get the GameState in Input.hs to test it or do it in Player.hs?

instance Player Human where
    chooseMove :: Human -> GameState -> IO Move
    -- the input system is context-aware, the human will just type in coords and the remaining
    -- info about the move will be filled in by the system
    chooseMove Human gs = do
        -- "while move is illegal: get move"
        move <- getMoveFRFRNoCap gs
        if isLegal gs move
            then do
                return move
            else do
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
    chooseMoveCapture :: Human -> GameState -> IO MoveCapture
    chooseMoveCapture Human gs = do
        -- "while move is illegal: get move"
        chosenMove <- chooseMove Human gs
        let newState = checkLegalAndResolve gs chosenMove
        let mayCapture = checkCapture newState chosenMove
        if mayCapture then do
            chosenCapture <- chooseCapture Human gs
            return $ MoveWithCapture chosenMove chosenCapture
        else do -- no capture
            return $ MoveWithoutCapture chosenMove

-- put this inside Player/chooseMove instead?

-- if a 2x2 square is not formed this function will leave the GameState as it is
-- needs a player to choose a piece to capture
-- the Move is the move that causes the capture, not the capturing move!
-- the GameState is the state after the move??
checkExecuteCapture :: (Player p) => p -> GameState -> Move -> GameState
checkExecuteCapture p gs move
    | checkCapture gs move = gs --TODO! change the gamestate with executeCapture
    | otherwise = gs




-- the heuristic will say how good a position is for White
-- or for a specified player?
-- TODO: or should it automatically take the player on the move from the GameState? Or the player who just made the move?
-- or maybe for white and black will try to minimize it?

-- goodness = # own stones / # enemy stones
-- not a subtraction because having 1 stone advantage is more important if there are fewer stones
-- ...at least in the "shift phase" 


heuristic :: GameState -> Float
heuristic (GameState b _ _ ) = getPlayerStonesFloat b White / getPlayerStonesFloat b Black
    where
        getPlayerStonesFloat :: Board -> Piece -> Float
        getPlayerStonesFloat b playerColor = int2Float $ getSpaceTypeNumber b (BoardField $ Just playerColor)


-- chooseStateFromChildren :: [(GameState, Float)] -> Piece -> (GameState, Float)
-- chooseStateFromChildren 


-- data StateAfterMove = StateAfterMove GameState Move

-- captureHappened :: StateAfterMove -> Bool
-- captureHappened = _

-- TODO: actually, shouldn't a Move contain the following capture if it occurs??
-- that would make the minimax simpler

-- int argument: depth of evaluation
minimaxGetBestMove :: GameState -> Int -> Piece -> Move
minimaxGetBestMove gs depth playerColor = do
    let moves = getPossibleMoves gs -- get available moves
    --let childStates = map (\m -> StateAfterMove (checkLegalAndResolve gs m) m) moves -- the legality check should be unnecessary, moves should contain only legal moves
    --let (childStatesCapture, childStatesNoCapture) = partitionByList (liftA2 checkCapture childStates) childStates
    --let childStatesAfterCapture

    -- TODO: problem: we need to simulate all possible captures as well
    -- and some moves will induce captures while others won't
    -- solution: replace child states that result in a capture with a set of child states after all possible captures
    -- though this will increase the branching factor considerably...
    moves!!0

--     return ()


data HeuristicAI = HeuristicAI

-- instance Player HeuristicAI where
--     chooseMove :: HeuristicAI -> GameState -> IO Move
--     chooseMove HeuristicAI (GameState b piece PhaseDrop) = _
--     chooseMove HeuristicAI (GameState b piece PhaseRemove) = _
--     chooseMove HeuristicAI (GameState b piece PhaseShift) = _ --TODO: get one of the player's stones that has a neighboring space free, move it there
--     chooseCapture :: HeuristicAI -> GameState -> IO Move
--     chooseCapture HeuristicAI (GameState b piece _) = _
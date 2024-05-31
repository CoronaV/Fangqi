module Player where
import Board (GameState (..), Phase (..), BoardField (..), Piece (..), Board)
import Moves (Move (..), getSpaceOfType, switchColor, checkCapture, isLegal, getSpaceTypeNumber)
import Input (getMove, getMoveFRFRNoCap)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative(liftA2))
import GHC.Float (int2Float)

-- "player" is an interface with a method for choosing moves
-- it returns IO Move - i.e. there is no guarantee that the same player will choose the same move twice
-- (necessary primarily to implement human players) 
class Player p where
    chooseMove :: p -> GameState -> IO Move -- a "regular move"
    chooseCapture :: p -> GameState -> IO Move --rename this 


data RandomAI = RandomAI

instance Player RandomAI where
    chooseMove :: RandomAI -> GameState -> IO Move
    -- if there is no legal move, we messed up, so the AI will just make whatever move e.g. (0,0)
    -- and cause an exception down the line
    -- play on the first free field from top left, if there is any
    chooseMove RandomAI (GameState b piece PhaseDrop) = return $ Drop piece $ fromMaybe (0,0) (getSpaceOfType b (BoardField Nothing) )
    chooseMove RandomAI (GameState b piece PhaseRemove) = return $ Remove piece $ fromMaybe (0,0) (getSpaceOfType b (BoardField (Just $ switchColor piece)) )
    chooseMove RandomAI (GameState b piece PhaseShift) = return $ Shift piece (0,0) (0,0) --TODO: get one of the player's stones that has a neighboring space free, move it there
    chooseCapture :: RandomAI -> GameState -> IO Move
    chooseCapture RandomAI (GameState b piece _) = chooseMove RandomAI (GameState b piece PhaseRemove)

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

    chooseCapture :: Human -> GameState -> IO Move
    chooseCapture Human (GameState b piece _) = chooseMove Human (GameState b piece PhaseRemove)


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



-- int argument: depth of evaluation
-- minimaxGetBestMove :: GameState -> Int -> Piece -> Move
-- minimaxGetBestMove depth playerColor = do
--     -- get available moves


--     return ()


data HeuristicAI = HeuristicAI

-- instance Player HeuristicAI where
--     chooseMove :: HeuristicAI -> GameState -> IO Move
--     chooseMove HeuristicAI (GameState b piece PhaseDrop) = _
--     chooseMove HeuristicAI (GameState b piece PhaseRemove) = _
--     chooseMove HeuristicAI (GameState b piece PhaseShift) = _ --TODO: get one of the player's stones that has a neighboring space free, move it there
--     chooseCapture :: HeuristicAI -> GameState -> IO Move
--     chooseCapture HeuristicAI (GameState b piece _) = _
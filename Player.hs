module Player where
import Board (GameState (..), Phase (..), BoardField (..))
import Moves (Move (..), getSpaceOfType, changeTurn, checkCapture)
import Input (getMove)
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative(liftA2))

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
    chooseMove RandomAI (GameState b piece PhaseRemove) = return $ Remove piece $ fromMaybe (0,0) (getSpaceOfType b (BoardField (Just $ changeTurn piece)) )
    chooseMove RandomAI (GameState b piece PhaseShift) = return $ Shift piece (0,0) (0,0) --TODO: get one of the player's stones that has a neighboring space free, move it there
    chooseCapture :: RandomAI -> GameState -> IO Move
    chooseCapture RandomAI (GameState b piece _) = return $ Remove piece $ fromMaybe (0,0) (getSpaceOfType b (BoardField (Just $ changeTurn piece)) )

data Human = Human --merge into one type with RandomAI?

instance Player Human where
    chooseMove :: Human -> GameState -> IO Move
    -- the input system is context-aware, the human will just type in coords and the remaining
    -- info about the move will be filled in by the system
    chooseMove Human (GameState b piece PhaseDrop) = fmap (Drop piece) getMove
    chooseMove Human (GameState b piece PhaseRemove) = fmap (Remove piece) getMove
    --need to implement a different prompt for getting the shift move coords...
    chooseMove Human (GameState b piece PhaseShift) = liftA2 (Shift piece) getMove getMove
    chooseCapture :: Human -> GameState -> IO Move
    chooseCapture Human (GameState b piece PhaseDrop) = fmap (Remove piece) getMove



-- put this inside Player/chooseMove instead?

-- if a 2x2 square is not formed this function will leave the GameState as it is
-- needs a player to choose a piece to capture
-- the Move is the move that causes the capture, not the capturing move!
-- the GameState is the state after the move??
checkExecuteCapture :: (Player p) => p -> GameState -> Move -> GameState
checkExecuteCapture p gs move
    | checkCapture gs move = gs --TODO! change the gamestate with executeCapture
    | otherwise = gs

module Player where
import Board (GameState (..), Phase (..), BoardField (..))
import Moves (Move (..), getSpaceOfType, changeTurn, checkCapture)
import Input (getMove)
import Data.Maybe (fromMaybe)

-- "player" is an interface with a method for choosing moves
-- it returns IO Move - i.e. there is no guarantee that the same player will choose the same move twice
-- (necessary primarily to implement human players) 
class Player p where
    makeMove :: p -> GameState -> IO Move

data RandomAI = RandomAI


instance Player RandomAI where
    makeMove :: RandomAI -> GameState -> IO Move
    -- if there is no legal move, we messed up, so the AI will just make whatever move and cause an exception down the line
    -- play on the first free field from top left, if there is any
    makeMove RandomAI (GameState b piece PhaseDrop) = return $ Drop piece $ fromMaybe (0,0) (getSpaceOfType b (BoardField Nothing) )
    makeMove RandomAI (GameState b piece PhaseRemove) = return $ Remove piece $ fromMaybe (0,0) (getSpaceOfType b (BoardField (Just $ changeTurn piece)) )
    makeMove RandomAI (GameState b piece PhaseShift) = return $ Shift piece (0,0) (0,0) --TODO: get one of the player's stones that has a neighboring space free, move it there

data Human = Human --merge into one type with RandomAI?

instance Player Human where
    makeMove :: Human -> GameState -> IO Move
    -- if there is no legal move, we messed up, so the AI will just make whatever move and cause an exception down the line
    -- play on the first free field from top left, if there is any
    makeMove Human (GameState b piece PhaseDrop) = getMove
    makeMove Human (GameState b piece PhaseRemove) = _
    makeMove Human (GameState b piece PhaseShift) = _


-- put this inside Player/makeMove instead?

-- if a 2x2 square is not formed this function will leave the GameState as it is
-- needs a player to choose a piece to capture
-- the Move is the move that causes the capture, not the capturing move!
-- the GameState is the state after the move??
checkExecuteCapture :: (Player p) => p -> GameState -> Move -> GameState
checkExecuteCapture p gs move
    | checkCapture gs move = gs --TODO! change the gamestate with executeCapture
    | otherwise = gs

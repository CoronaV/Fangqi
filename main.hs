import Board
    (Piece(..), emptyBoard, GameState (..), Phase (..), displayGameState )
import Player (Player (..), Human (..), HeuristicAI (..), DummyAI(..))
import Moves (switchColor, MoveCapture (..), checkLegalAndResolveMC, gamestateCheckGameEnd)


-- use "do" here to prevent the IO monad from getting everywhere (like move validity checking where it's not appropriate)
playGame :: (Player p1, Player p2) => p1 -> p2 -> (Int,Int) -> IO ()
playGame p1 p2 (i,j) = do
    displayGameState startState
    playGameInternal p1 p2 startState
    where startState = GameState (emptyBoard i j) White PhaseDrop

playGameInternal :: (Player p1, Player p2) => p1 -> p2 -> GameState -> IO ()
playGameInternal p1 p2 current = do
    let gameEnded = gamestateCheckGameEnd current -- phase ends are handled by checkLegalAndResolveMC
    if gameEnded
        then do
        putStrLn "End of game. The result is:"
        putStrLn (evaluateEndPosition current)
    else do
        chosenMoveCapture <- chooseMoveCapture p1 current
        let afterMoveCapture = checkLegalAndResolveMC current chosenMoveCapture
        displayGameState afterMoveCapture
        playGameInternal p2 p1 afterMoveCapture


evaluateEndPosition :: GameState -> String
evaluateEndPosition gs = do
    let gameEnded = gamestateCheckGameEnd gs
    if gameEnded then do
        let winner = show (switchColor (piece gs)) -- the player that should move can't move -> win for the other player
        winner ++ " won the game!"
    else do
        error "Game ended prematurely."
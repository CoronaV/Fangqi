module Tests where
import Board (Board, GameState(..), emptyBoard, Piece (..), Phase (..), BoardField (..), emptyRow, isLeftUpCornerOfSquare)
import Player (RandomAI(..), Human (..), HeuristicAI (HeuristicAI), minimaxGetBestMove, Player (..), minimaxMoveGetter)
import Main ( playGame, evaluateEndPosition, playGameInit )
import Moves (Move (..), getPossibleMoves, getPossibleMCs, MoveCapture (..), getCapturesIfApplicable, getPossibleCaptures, checkCaptureAfter, checkCaptureBefore, checkLegalAndResolveMC, dropPhaseEndCheck, nextPhase, gamestateCheckPhaseEnd)

--debugging:
chessBoard :: Board
chessBoard = emptyBoard 4 4

startState :: GameState
startState = GameState chessBoard White PhaseDrop


-- sampleGameDropPhase :: IO GameState
-- sampleGameDropPhase = playPhase HeuristicAI HeuristicAI startState


-- sampleGameRemovePhase :: IO GameState
-- sampleGameRemovePhase = do
--     start <- sampleGameDropPhase
--     playPhase RandomAI RandomAI (nextPhase start)


-- humanGameDropPhase :: IO GameState
-- humanGameDropPhase = playPhase Human Human startState

-- humanAIGameDropPhase :: IO GameState
-- humanAIGameDropPhase = playPhase Human RandomAI startState

oneStoneBoard :: Board
oneStoneBoard = [emptyRow 3, [BoardField Nothing, BoardField (Just Black), BoardField Nothing], emptyRow 3]
testPossMoves :: [Move]
testPossMoves = getPossibleMoves (GameState oneStoneBoard Black PhaseShift)



minimaxTestBoard :: Board
minimaxTestBoard = [
                    [BoardField (Just Black), BoardField (Just Black), BoardField Nothing],
                    [BoardField (Just White), BoardField (Just White), BoardField Nothing],
                    [BoardField (Just White), BoardField Nothing, BoardField Nothing]
                    ]

testState :: GameState
testState = GameState minimaxTestBoard White PhaseDrop


checkAlphaBeta :: (MoveCapture, Float)
checkAlphaBeta = minimaxMoveGetter 3 testState

chAB2 = minimaxGetBestMove 2 (checkLegalAndResolveMC testState (MoveWithoutCapture (Drop White (0,2))) )

gg = checkLegalAndResolveMC testState (MoveWithoutCapture (Drop White (0,2)))

--chAB3 = minimaxGetBestMove 1 (checkLegalAndResolveMC testState (MoveWithoutCapture (Drop White (0,2))) )



-- minimaxTestDropPhase :: IO GameState
-- minimaxTestDropPhase = playPhase HeuristicAI HeuristicAI testState


possibleMCs :: [MoveCapture]
possibleMCs = getPossibleMCs testState

hh :: [MoveCapture]
hh = getCapturesIfApplicable testState (Drop White (2,1))


aa :: [MoveCapture]
aa = getCapturesIfApplicable testState (Drop White (2,1))

bb :: Bool
bb = checkCaptureBefore testState (Drop White (2,1))

cc :: Bool
cc = isLeftUpCornerOfSquare minimaxTestBoard White (1,0)


fullBoard ::Board
fullBoard = [
            [BoardField (Just Black), BoardField (Just Black), BoardField (Just Black)],
            [BoardField (Just White), BoardField (Just White), BoardField (Just White)],
            [BoardField (Just White), BoardField (Just White), BoardField (Just Black)]
            ]


fbTest :: (MoveCapture, Float)
fbTest = minimaxMoveGetter 0 (GameState fullBoard White PhaseDrop)


overallTest :: IO ()
overallTest = playGameInit Human HeuristicAI (7,7)

smallBoardTest :: IO()
smallBoardTest = playGameInit HeuristicAI Human (4,4)

--need: better console interaction for announcing captures/AI moves, input error msg, correct order of turn message, board
-- define both heuristics and mechanics for loss if no moves available and phase end conditions not met

--shift move legality for human players??

--remove unused imports

myBoard ::Board
myBoard = [
            [BoardField (Just White), BoardField (Just White), BoardField (Just Black)],
            [BoardField (Just White), BoardField (Just Black), BoardField (Just White)],
            [BoardField (Just White), BoardField (Nothing), BoardField (Just White)]
            ]

ggss :: GameState
ggss = GameState myBoard Black PhaseShift


aaaa :: Bool
aaaa = gamestateCheckPhaseEnd ggss
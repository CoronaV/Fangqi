module Tests where
import Board (Board, GameState(..), emptyBoard, Piece (..), Phase (..), BoardField (..), emptyRow, isLeftUpCornerOfSquare)
import Player (RandomAI(..), Human (..), HeuristicAI (HeuristicAI), minimaxGetBestMove, Player (..))
import Main (playGame, dropPhaseEndCheck, removePhaseEndCheck, nextPhase)
import Moves (Move (..), getPossibleMoves, getPossibleMCs, MoveCapture (..), getCapturesIfApplicable, getPossibleCaptures, checkCaptureAfter, checkCaptureBefore, checkLegalAndResolveMC)

--debugging:
chessBoard :: Board
chessBoard = emptyBoard 4 4

startState :: GameState
startState = GameState chessBoard White PhaseDrop


sampleGameDropPhase :: IO GameState
sampleGameDropPhase = playGame HeuristicAI HeuristicAI dropPhaseEndCheck startState


sampleGameRemovePhase :: IO GameState
sampleGameRemovePhase = do
    start <- sampleGameDropPhase
    playGame RandomAI RandomAI removePhaseEndCheck (nextPhase start)


humanGameDropPhase :: IO GameState
humanGameDropPhase = playGame Human Human dropPhaseEndCheck startState

humanAIGameDropPhase :: IO GameState
humanAIGameDropPhase = playGame Human RandomAI dropPhaseEndCheck startState

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
checkAlphaBeta = minimaxGetBestMove 3 testState

chAB2 = minimaxGetBestMove 2 (checkLegalAndResolveMC testState (MoveWithoutCapture (Drop White (0,2))) )

gg = checkLegalAndResolveMC testState (MoveWithoutCapture (Drop White (0,2)))

--chAB3 = minimaxGetBestMove 1 (checkLegalAndResolveMC testState (MoveWithoutCapture (Drop White (0,2))) )



minimaxTestDropPhase :: IO GameState
minimaxTestDropPhase = playGame HeuristicAI HeuristicAI dropPhaseEndCheck testState


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
fbTest = minimaxGetBestMove 0 (GameState fullBoard White PhaseDrop)
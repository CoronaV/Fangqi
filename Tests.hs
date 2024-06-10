module Tests where
import Board (Board, GameState(..), emptyBoard, Piece (..), Phase (..), BoardField (..), emptyRow, isLeftUpCornerOfSquare)
import Player (RandomAI(..), Human (..), HeuristicAI (HeuristicAI), minimaxGetBestMove, Player (..), minimaxMoveGetter)
import Main (playPhase, playGame, evaluateEndPosition)
import Moves (Move (..), getPossibleMoves, getPossibleMCs, MoveCapture (..), getCapturesIfApplicable, getPossibleCaptures, checkCaptureAfter, checkCaptureBefore, checkLegalAndResolveMC, dropPhaseEndCheck, nextPhase)

--debugging:
chessBoard :: Board
chessBoard = emptyBoard 4 4

startState :: GameState
startState = GameState chessBoard White PhaseDrop


sampleGameDropPhase :: IO GameState
sampleGameDropPhase = playPhase HeuristicAI HeuristicAI startState


sampleGameRemovePhase :: IO GameState
sampleGameRemovePhase = do
    start <- sampleGameDropPhase
    playPhase RandomAI RandomAI (nextPhase start)


humanGameDropPhase :: IO GameState
humanGameDropPhase = playPhase Human Human startState

humanAIGameDropPhase :: IO GameState
humanAIGameDropPhase = playPhase Human RandomAI startState

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



minimaxTestDropPhase :: IO GameState
minimaxTestDropPhase = playPhase HeuristicAI HeuristicAI testState


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
overallTest = playGame Human HeuristicAI (7,7)

smallBoardTest :: IO()
smallBoardTest = playGame HeuristicAI Human (3,3)

--need: better console interaction for announcing captures/AI moves, input error msg, correct order of turn message, board
-- support for one-line shift moves? "b5 d5"
-- choose top 15 moves by heuristics (number of pieces next to it), then minimax among them
-- implement repetition draws, define both heuristics and mechanics for loss if no moves available and phase end conditions not met

--shift move legality for human players??

--switching turns between phases

--remove unused imports

-- Why didn't the AI see this coming? Minimax depth 3

-- It is now Black's turn in the phase PhaseDrop
-- Make a move.Drop piece to: d5
-- 1|w w w w .
-- 2|w w b b w
-- 3|. b w b .
-- 4|. b b b .
-- 5|. w . b .
--  ----------
--   a b c d e
-- It is now White's turn in the phase PhaseDrop
-- 1|w w w w w
-- 2|w w b b w
-- 3|. b w b .
-- 4|. b b b .
-- 5|. w . b .
--  ----------
--   a b c d e
-- It is now Black's turn in the phase PhaseDrop
-- Make a move.Drop piece to: c5

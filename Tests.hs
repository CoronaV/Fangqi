module Tests where
import Board (Board, GameState(..), emptyBoard, Piece (..), Phase (..), BoardField (..), emptyRow, isLeftUpCornerOfSquare)
import Player (DummyAI(..), Human (..), HeuristicAI (HeuristicAI), minimaxGetBestMove, Player (..), minimaxMoveGetter)
import Main ( playGame, evaluateEndPosition, playGame )
import Moves (Move (..), getPossibleMoves, getPossibleMCs, MoveCapture (..), getCapturesIfApplicable, getPossibleCaptures, checkCaptureAfter, checkCaptureBefore, checkLegalAndResolveMC, dropPhaseEndCheck, nextPhase, gamestateCheckPhaseEnd)

--debugging module
chessBoard :: Board
chessBoard = emptyBoard 4 4

startState :: GameState
startState = GameState chessBoard White PhaseDrop

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

checkAlphaBeta2 :: [MoveCapture] -> (MoveCapture, Float)
checkAlphaBeta2 = minimaxGetBestMove 2 (checkLegalAndResolveMC testState (MoveWithoutCapture (Drop White (0,2))) )

checkLegality :: GameState
checkLegality = checkLegalAndResolveMC testState (MoveWithoutCapture (Drop White (0,2)))

possibleMCs :: [MoveCapture]
possibleMCs = getPossibleMCs testState

availableMoveTest :: [MoveCapture]
availableMoveTest = getCapturesIfApplicable testState (Drop White (2,1))

availableMCTest :: [MoveCapture]
availableMCTest = getCapturesIfApplicable testState (Drop White (2,1))

captureChecking :: Bool
captureChecking = checkCaptureBefore testState (Drop White (2,1))

captureSquareChecking :: Bool
captureSquareChecking = isLeftUpCornerOfSquare minimaxTestBoard White (1,0)

fullBoard ::Board
fullBoard = [
            [BoardField (Just Black), BoardField (Just Black), BoardField (Just Black)],
            [BoardField (Just White), BoardField (Just White), BoardField (Just White)],
            [BoardField (Just White), BoardField (Just White), BoardField (Just Black)]
            ]


noMovesAvailableTest :: (MoveCapture, Float)
noMovesAvailableTest = minimaxMoveGetter 0 (GameState fullBoard White PhaseDrop)

overallTest :: IO ()
overallTest = playGame Human HeuristicAI (7,7)

smallBoardTest :: IO()
smallBoardTest = playGame HeuristicAI Human (4,4)

endgameSituation ::Board
endgameSituation = [
            [BoardField (Just White), BoardField (Just White), BoardField (Just Black)],
            [BoardField (Just White), BoardField (Just Black), BoardField (Just White)],
            [BoardField (Just White), BoardField (Nothing), BoardField (Just White)]
            ]

endgameState :: GameState
endgameState = GameState endgameSituation Black PhaseShift


endGameTest :: Bool
endGameTest = gamestateCheckPhaseEnd testState
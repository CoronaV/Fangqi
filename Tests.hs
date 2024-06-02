module Tests where
import Board (Board, GameState(..), emptyBoard, Piece (..), Phase (PhaseDrop))
import Player (RandomAI(..), Human (..))
import Main (playGame, dropPhaseEndCheck, removePhaseEndCheck, nextPhase)

--debugging:
chessBoard :: Board
chessBoard = emptyBoard 8 8

startState :: GameState
startState = GameState chessBoard White PhaseDrop


sampleGameDropPhase :: IO GameState
sampleGameDropPhase = playGame RandomAI RandomAI dropPhaseEndCheck startState


sampleGameRemovePhase :: IO GameState
sampleGameRemovePhase = do
    start <- sampleGameDropPhase
    playGame RandomAI RandomAI removePhaseEndCheck (nextPhase start)


humanGameDropPhase :: IO GameState
humanGameDropPhase = playGame Human Human dropPhaseEndCheck startState

humanAIGameDropPhase :: IO GameState
humanAIGameDropPhase = playGame Human RandomAI dropPhaseEndCheck startState
module Tetris where

import Tetris.GameLogic
import Tetris.Render
import CodeWorld
import TimeQuantizer


-- | Wrap game logic into quantizator
initTetris = quantizeInit initGameState
updateTetris = quantizeUpdate updateGameState 1.0
renderTetris = quantizeRender renderGameState
handleTetris = quantizeHandler handleGameState


run :: IO ()
run = interactionOf initTetris updateTetris handleTetris renderTetris 

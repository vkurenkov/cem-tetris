module Tetris where

import Tetris.GameLogic
import Tetris.Render
import CodeWorld
import TimeQuantizer
import System.Random

randomSeed = 10

run :: IO ()
run = do
  let g = mkStdGen 42
  getInteraction g

getInteraction :: StdGen -> IO ()
getInteraction gen
  = interactionOf initTetris updateTetris handleTetris renderTetris
  where
    initTetris = quantizeInit (initGeneralGameState gen)
    updateTetris = quantizeUpdate updateGeneralGameState 0.1
    renderTetris = quantizeRender renderGeneralGameState
    handleTetris = quantizeHandler handleGeneralGameState

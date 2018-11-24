module Tetris where

import Tetris.GameLogic
import Tetris.Render
import CodeWorld
import TimeQuantizer
import System.Random

run :: IO ()
run = do
  let g = mkStdGen 20
  -- getUserInteraction g
  getBotInteraction g

getConcurrentInteraction :: StdGen -> IO ()
getConcurrentInteraction gen
  = interactionOf initTetris updateTetris handleTetris renderTetris
  where
    initTetris = quantizeInit (initGeneralGameState gen)
    updateTetris = quantizeUpdate updateGeneralGameState 0.002
    renderTetris = quantizeRender renderGeneralGameState
    handleTetris = quantizeHandler handleGeneralGameState

getUserInteraction :: StdGen -> IO ()
getUserInteraction gen
  = interactionOf initTetris updateTetris handleTetris renderTetris
  where
    initTetris = quantizeInit (initGameState gen)
    updateTetris = quantizeUpdate updateGameState 0.2
    renderTetris = quantizeRender renderUserGameState
    handleTetris = quantizeHandler handleUserGameState

getBotInteraction :: StdGen -> IO ()
getBotInteraction gen
  = interactionOf initTetris updateTetris handleTetris renderTetris
  where
    initTetris = quantizeInit (initGameState gen)
    updateTetris = quantizeUpdate updateBotGameState 0.002
    renderTetris = quantizeRender renderBotGameState
    handleTetris = quantizeHandler handleBotGameState

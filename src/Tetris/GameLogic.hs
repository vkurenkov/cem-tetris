module Tetris.GameLogic where

import CodeWorld
import Tetris.GameTypes

fallSpeed :: Velocity
fallSpeed = (0, -1)

initGameState :: GameState
initGameState = GameState (Field 22 10) (Just (getTetromino O)) []

handleGameState :: Event -> GameState -> GameState
handleGameState _ = id

updateGameState :: GameState -> GameState
updateGameState (GameState field tetromino cells)
  | isFinished curState = curState
  | otherwise = GameState field (moveTetromino fallSpeed tetromino) cells
  where
    curState = (GameState field tetromino cells)

moveTetromino :: Velocity -> Maybe Tetromino -> Maybe Tetromino
moveTetromino _ Nothing = Nothing
moveTetromino (dx, dy) (Just (Tetromino (x, y) cells))
  = Just (Tetromino (x + dx, y + dy) cells)

handleCollision :: GameState -> GameState
handleCollision = id

removeFilledRows :: GameState -> GameState
removeFilledRows = id

generateTetromino :: GameState -> GameState
generateTetromino = id

isFinished :: GameState -> Bool
isFinished _ = False

getTetromino :: Type -> Tetromino
getTetromino O = (Tetromino (5, 5) [(RelativeCell (x, y) yellow) | x <- [0,1], y <- [0,1]])
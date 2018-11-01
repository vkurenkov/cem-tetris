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
updateGameState gs
  | isFinished gs = gs
  | otherwise = (removeFilledRows
               . generateTetromino
               . handleCollision
               . moveTetromino ) gs


-- / Moves tetromino according to velocity
moveTetromino :: GameState -> GameState
moveTetromino (GameState field Nothing cells)
  = (GameState field Nothing cells)
moveTetromino (GameState field (Just tetromino) cells)
  = (GameState field movedTetromino cells)
  where
    (Tetromino (x, y) rCells) = tetromino
    (dx, dy) = fallSpeed

    movedTetromino = Just (Tetromino (x + dx, y + dy) rCells)


-- / Check position of tetromino, if touches objects or bottom line add to cells
handleCollision :: GameState -> GameState
handleCollision (GameState field Nothing cells)
  = GameState field Nothing cells
handleCollision (GameState field (Just tetromino) cells)
  | touchesBottom = GameState field Nothing cells
--  | touchesCells =
  | otherwise = GameState field (Just tetromino) cells
  where
    (Tetromino (posX, posY) rCells) = tetromino
    absCells = toCells (posX, posY) rCells

    touchesBottom
      | posY == 0 = True
      | otherwise = False

    touchesCells = False
    
    toCells :: Position -> [RelativeCell] -> [Cell]
    toCells (x, y)
      = map (\(RelativeCell (dx, dy) c) -> Cell (x + dx, y + dy) c)


-- onlyRows :: [Cell] -> [Integer]
-- onlyRows = map (\(Cell (_, y) _) -> y)
--
-- cellsInCol :: Integer -> [Cell] -> [Cell]
-- cellsInCol col = filter (\(Cell (x, _ ) _) -> x == col)
--
--

--
-- ff = [(Cell (10, 1) yellow),
--       (Cell (10, 2) yellow),
--       (Cell (10, 5) yellow),
--       (Cell (4, 1) yellow),
--       (Cell (5, 2) yellow)]


removeFilledRows :: GameState -> GameState
removeFilledRows = id

generateTetromino :: GameState -> GameState
generateTetromino = id

isFinished :: GameState -> Bool
isFinished _ = False

getTetromino :: Type -> Tetromino
getTetromino O = (Tetromino (5, 5) [(RelativeCell (x, y) yellow) | x <- [0,1], y <- [0,1]])

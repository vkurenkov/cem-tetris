{-# LANGUAGE OverloadedStrings #-}
module Tetris.GameLogic where

import CodeWorld
import Tetris.GameTypes

fallSpeed :: Velocity
fallSpeed = (0, -1)

initGameState :: GameState
initGameState = GameState (Field 22 10) (Just (getTetromino Z)) []

handleGameState :: Event -> GameState -> GameState
handleGameState (KeyPress "Up") (GameState field tetromino cells)
  = GameState field (rotateTetromino tetromino) cells
handleGameState _ (GameState field Nothing cells)
  = (GameState field Nothing cells)
handleGameState _ gameState
  = gameState

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


-- | Tetromino factory based on its type
getTetromino :: Type -> Tetromino
getTetromino I = (Tetromino (5, 23) [(RelativeCell pos (light blue)) | pos <- [(-1, 0), (0, 0), (1, 0), (2, 0)]])
getTetromino J = (Tetromino (5, 23) [(RelativeCell pos blue) | pos <- [(-1, 0), (0, 0), (1, 0), (-1, 1)]])
getTetromino L = (Tetromino (5, 23) [(RelativeCell pos orange) | pos <- [(-1, 0), (0, 0), (1, 0), (1, 1)]])
getTetromino O = (Tetromino (5, 23) [(RelativeCell (x, y) yellow) | x <- [0,1], y <- [0,1]])
getTetromino S = (Tetromino (5, 23) [(RelativeCell pos green) | pos <- [(-1, 0), (0, 0), (1, 1), (1, 1)]])
getTetromino T = (Tetromino (5, 23) [(RelativeCell pos purple) | pos <- [(-1, 0), (0, 0), (1, 0), (0, 1)]])
getTetromino Z = (Tetromino (5, 23) [(RelativeCell pos red) | pos <- [(-1, 1), (0, 0), (0, 1), (1, 0)]])


-- | Rotate tetromino (defined using rotation matrix https://en.wikipedia.org/wiki/Rotation_matrix)
rotateTetromino :: Maybe Tetromino -> Maybe Tetromino
rotateTetromino Nothing = Nothing
rotateTetromino (Just (Tetromino pos relCells))
  | isSquare = Just (Tetromino pos relCells)
  | otherwise = Just (Tetromino pos rotatedCells)
  where
    rotatedCells = map (\(RelativeCell (x, y) c) -> (RelativeCell (y, -x) c)) relCells
    isSquare
      = elem (0, 0) relPositions
      && elem (1, 0) relPositions
      && elem (0, 1) relPositions
      && elem (1, 1) relPositions
    relPositions = map (\(RelativeCell pos c) -> pos) relCells

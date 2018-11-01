{-# LANGUAGE OverloadedStrings #-}
module Tetris.GameLogic where

import CodeWorld
import Tetris.GameTypes

fallSpeed :: Velocity
fallSpeed = (0, -1)

initGameState :: GameState
initGameState = GameState (Field 22 10) Nothing []

handleGameState :: Event -> GameState -> GameState
-- | Handle tetromino rotation
handleGameState (KeyPress "Up") (GameState field (Just tetromino) cells)
  = GameState field newTetromino cells
  where
    newTetromino
      | intersects rotatedTetromino cells = Just tetromino
      | outOfWidth field rotatedTetromino = Just tetromino
      | underBottom rotatedTetromino = Just tetromino
      | otherwise = Just rotatedTetromino
    rotatedTetromino = rotateTetromino tetromino
-- | Handle tetromino move left
handleGameState (KeyPress "Left") (GameState field (Just tetromino) cells)
  = GameState field newTetromino cells
  where
    newTetromino
      | intersects movedTetromino cells = Just tetromino
      | outOfWidth field movedTetromino = Just tetromino
      | otherwise = Just movedTetromino
    movedTetromino = offsetTetromino (-1, 0) tetromino
-- | Handle tetromino move left
handleGameState (KeyPress "Right") (GameState field (Just tetromino) cells)
  = GameState field newTetromino cells
  where
    newTetromino
      | intersects movedTetromino cells = Just tetromino
      | outOfWidth field movedTetromino = Just tetromino
      | otherwise = Just movedTetromino
    movedTetromino = offsetTetromino (1, 0) tetromino
-- | Handle tetromino move down
handleGameState (KeyPress "Down") gameState
  = updateGameState gameState
-- | All other cases
handleGameState _ (GameState field Nothing cells)
  = (GameState field Nothing cells)
handleGameState _ gameState
  = gameState

updateGameState :: GameState -> GameState
updateGameState gs
  | isFinished gs = gs
  | otherwise = (removeFilledRows
               . fallTetromino
               . handleCollision
               . generateTetromino) gs

-- / Moves tetromino according to velocity
offsetTetromino :: Velocity -> Tetromino -> Tetromino
offsetTetromino (dx, dy) (Tetromino (x, y) rCells)
  = Tetromino (x + dx, y + dy) rCells

fallTetromino :: GameState -> GameState
fallTetromino (GameState field Nothing cells)
  = (GameState field Nothing cells)
fallTetromino (GameState field (Just tetromino) cells)
  = (GameState field offsetedTetromino cells)
  where
    offsetedTetromino = Just (offsetTetromino fallSpeed tetromino)

-- / Check position of tetromino, if touches objects or bottom line add to cells
handleCollision :: GameState -> GameState
handleCollision (GameState field Nothing cells)
  = GameState field Nothing cells
handleCollision (GameState field (Just tetromino) cells)
  | touchesBottom = GameState field Nothing newCells
  | touchesCells = GameState field Nothing newCells
  | otherwise = GameState field (Just tetromino) cells
  where
    (Tetromino (posX, posY) rCells) = tetromino
    possibleTetromino = (Tetromino (posX, posY - 1) rCells)

    touchesCells = intersects possibleTetromino cells
    touchesBottom = underBottom possibleTetromino
    newCells = mergeTetrominoAndCells tetromino cells

mergeTetrominoAndCells :: Tetromino -> [Cell] -> [Cell]
mergeTetrominoAndCells (Tetromino position relativeCells) cs
  = cs ++ (relativeToCells position relativeCells)

intersects :: Tetromino -> [Cell] -> Bool
intersects (Tetromino position relativeCells) cells
  = foldr (||) False (map (\(c1, c2) -> eqCells c1 c2) allCellsCombinations)
    where
      absCells = relativeToCells position relativeCells
      allCellsCombinations = [(c1, c2) | c1  <- absCells, c2 <- cells]

      eqCells :: Cell -> Cell -> Bool
      eqCells (Cell (x1, y1) _) (Cell (x2, y2) _)
        = x1 == x2 && y1 == y2

outOfWidth :: Field -> Tetromino -> Bool
outOfWidth (Field _ width) (Tetromino pos rCells)
  = minx < 0 || maxx >= width
  where
    maxx = maximum xs
    minx = minimum xs
    xs = map (\(Cell (x, _) _) -> x) (relativeToCells pos rCells)

underBottom :: Tetromino -> Bool
underBottom (Tetromino position cells)
  = foldr (||) False (map (\(Cell (_, y) _) -> y < 0) possibleCells)
    where
      possibleCells = relativeToCells position cells


relativeToCells :: Position -> [RelativeCell] -> [Cell]
relativeToCells (x, y)
  = map (\(RelativeCell (dx, dy) c) -> Cell (x + dx, y + dy) c)


removeFilledRows :: GameState -> GameState
removeFilledRows (GameState field tetromino cells)
  = (GameState field tetromino newCells)
  where
    (Field _ width ) = field

    zeroSplit = splitCells 0 cells
    (s1, s2, s3) = removeAndShift 0 zeroSplit
    newCells = s1 ++ s2 ++ s3

    removeAndShift :: Integer
                    -> ([Cell], [Cell], [Cell])
                    -> ([Cell], [Cell], [Cell])
    removeAndShift _ (below, [], [])
      = (below, [], [])
    removeAndShift _ (below, at, [])
      | isFull at = (below, [], [])
      | otherwise = (below, at, [])
    removeAndShift rowToSplit (below, at, above)
      | isFull at
        = removeAndShift rowToSplit
          (splitCells rowToSplit (below ++ (shiftCells above)))
      | otherwise
        = removeAndShift (rowToSplit + 1) (splitCells (rowToSplit + 1) (above ++ at ++ below))

    shiftCells :: [Cell] -> [Cell]
    shiftCells = map (\(Cell (x, y) c) -> Cell (x, y-1) c)

    isFull :: [Cell] -> Bool
    isFull cs = toInteger (length cs) == width

    splitCells :: Integer -> [Cell] -> ([Cell], [Cell], [Cell])
    splitCells row cs
      = (below, at, above)
        where
          above = filter (\(Cell (_, y) _) -> y > row) cs
          at    = filter (\(Cell (_, y) _) -> y == row) cs
          below = filter (\(Cell (_, y) _) -> y < row) cs



    -- detect cell with all row




generateTetromino :: GameState -> GameState
generateTetromino (GameState field Nothing cells)
  = (GameState field (Just (getTetromino L)) cells)
generateTetromino gs = gs


isFinished :: GameState -> Bool
isFinished (GameState (Field height _) _ cells)
  = foldr (||) False (map (\(Cell (_, y) _) -> y >= height-1) cells)


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
rotateTetromino :: Tetromino -> Tetromino
rotateTetromino (Tetromino pos relCells)
  | isSquare = Tetromino pos relCells
  | otherwise = Tetromino pos rotatedCells
  where
    rotatedCells = map (\(RelativeCell (x, y) c) -> (RelativeCell (y, -x) c)) relCells
    isSquare
      = elem (0, 0) relPositions
      && elem (1, 0) relPositions
      && elem (0, 1) relPositions
      && elem (1, 1) relPositions
    relPositions = map (\(RelativeCell pos c) -> pos) relCells

{-# LANGUAGE OverloadedStrings #-}
module Tetris.GameLogic where

import CodeWorld
import Tetris.GameTypes
import System.Random
import Data.Random.Normal

fallOffset :: Offset
fallOffset = (0, -1)

initGameState :: StdGen -> GameState
initGameState gen = GameState (Field 22 10) Nothing [] gen 0

-- | Handles input events
handleGameState :: Event -> GameState -> GameState
handleGameState event gameState = applyGameAction action gameState
  where
    action
      | event == (KeyPress "Up") = Just Rotate
      | event == (KeyPress "Down") = Just MoveDown
      | event == (KeyPress "Left") = Just MoveLeft
      | event == (KeyPress "Right") = Just MoveRight
      | otherwise = Nothing

-- | Applies given game action
applyGameAction :: Maybe GameAction -> GameState -> GameState
applyGameAction Nothing gameState = gameState
applyGameAction _ (GameState field Nothing cells seed score) = GameState field Nothing cells seed score
applyGameAction (Just MoveDown) gameState = updateGameState gameState
applyGameAction (Just action) (GameState field (Just tetromino) cells seed score)
  = GameState field newTetromino cells seed score
  where
    newTetromino
      | intersects tetrominoAfterApply cells = Just tetromino
      | outOfWidth field tetrominoAfterApply = Just tetromino
      | underBottom tetrominoAfterApply = Just tetromino
      | otherwise = Just tetrominoAfterApply
    tetrominoAfterApply
      | action == MoveLeft = offsetTetromino (-1, 0) tetromino
      | action == MoveRight = offsetTetromino (1, 0) tetromino
      | action == Rotate = rotateTetromino tetromino
      | otherwise = tetromino

-- | Updates current game state
updateGameState :: GameState -> GameState
updateGameState gs
  | isFinished gs = gs
  | otherwise = (removeFilledRows
               . fallTetromino
               . handleCollision
               . generateTetromino) gs

-- | Offsets tetromino by given offset
offsetTetromino :: Offset -> Tetromino -> Tetromino
offsetTetromino (dx, dy) (Tetromino (x, y) rCells)
  = Tetromino (x + dx, y + dy) rCells

-- | Game update for falling tetromino
fallTetromino :: GameState -> GameState
fallTetromino (GameState field Nothing cells seed score)
  = (GameState field Nothing cells seed score)
fallTetromino (GameState field (Just tetromino) cells seed score)
  = (GameState field offsetedTetromino cells seed score)
  where
    offsetedTetromino = Just (offsetTetromino fallOffset tetromino)

-- | Check position of tetromino, if touches objects or bottom line add to cells
handleCollision :: GameState -> GameState
handleCollision (GameState field Nothing cells seed score)
  = GameState field Nothing cells seed score
handleCollision (GameState field (Just tetromino) cells seed score)
  | touchesBottom = GameState field Nothing newCells seed score
  | touchesCells = GameState field Nothing newCells seed score
  | otherwise = GameState field (Just tetromino) cells seed score
  where
    (Tetromino (posX, posY) rCells) = tetromino
    possibleTetromino = Tetromino (posX, posY - 1) rCells

    touchesCells = intersects possibleTetromino cells
    touchesBottom = underBottom possibleTetromino
    newCells = mergeTetrominoAndCells tetromino cells

-- | Merges tetromino and current cells on the field
mergeTetrominoAndCells :: Tetromino -> [Cell] -> [Cell]
mergeTetrominoAndCells (Tetromino position relativeCells) cs
  = cs ++ relativeToCells position relativeCells

-- | Checks whether the given tetromino intersects with the cells on the field
intersects :: Tetromino -> [Cell] -> Bool
intersects (Tetromino position relativeCells) cells
  = foldr (||) False (map (\(c1, c2) -> eqCells c1 c2) allCellsCombinations)
    where
      absCells = relativeToCells position relativeCells
      allCellsCombinations = [(c1, c2) | c1  <- absCells, c2 <- cells]

      eqCells :: Cell -> Cell -> Bool
      eqCells (Cell (x1, y1) _) (Cell (x2, y2) _)
        = x1 == x2 && y1 == y2

-- | Checks whether the given tetromino is out of the borders of the field in x-axis
outOfWidth :: Field -> Tetromino -> Bool
outOfWidth (Field _ width) (Tetromino pos rCells)
  = minx < 0 || maxx >= width
  where
    maxx = maximum xs
    minx = minimum xs
    xs = map (\(Cell (x, _) _) -> x) (relativeToCells pos rCells)

-- | Checks whether the given tetromino is under the bottom of the field
underBottom :: Tetromino -> Bool
underBottom (Tetromino position cells)
  = foldr (||) False (map (\(Cell (_, y) _) -> y < 0) possibleCells)
    where
      possibleCells = relativeToCells position cells

-- | Converts relative cells to world cells (local to world positions conversion)
relativeToCells :: Position -> [RelativeCell] -> [Cell]
relativeToCells (x, y)
  = map (\(RelativeCell (dx, dy) c) -> Cell (x + dx, y + dy) c)

-- | Remove rows that are fully filled and move other cells accordingly
removeFilledRows :: GameState -> GameState
removeFilledRows (GameState field tetromino cells seed score)
  = (GameState field tetromino newCells seed newScore)
  where
    (Field _ width ) = field

    zeroSplit = splitCells 0 cells
    (s1, s2, s3) = removeAndShift 0 zeroSplit
    newCells = s1 ++ s2 ++ s3

    -- deltaLines = floor (fromInteger ((length cells) - (length newCells)) / width)
    deltaLines = floor ((fromIntegral ((length cells) - (length newCells)))
                  / (fromIntegral width))
    newScore = score + (evaluateScore deltaLines)

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

    evaluateScore :: Integer -> Integer
    evaluateScore 0 = 0
    evaluateScore 1 = 40
    evaluateScore 2 = 100
    evaluateScore 3 = 300
    evaluateScore 4 = 1200
    evaluateScore _ = 0



-- | Generates new tetromino if there is nothing
generateTetromino :: GameState -> GameState
generateTetromino (GameState field Nothing cells gen score)
  = GameState field (intToTetromino rNumber) cells nGen score
  where
    (rNumber, nGen) = randomR (0,6) gen

    intToTetromino :: Integer -> Maybe Tetromino
    intToTetromino 0 = Just (getTetromino Z)
    intToTetromino 1 = Just (getTetromino L)
    intToTetromino 2 = Just (getTetromino O)
    intToTetromino 3 = Just (getTetromino S)
    intToTetromino 4 = Just (getTetromino I)
    intToTetromino 5 = Just (getTetromino J)
    intToTetromino 6 = Just (getTetromino T)
    intToTetromino _ = Nothing
generateTetromino gs = gs

-- | Checks whether the game is finished or not
isFinished :: GameState -> Bool
isFinished (GameState (Field height _) _ cells _ _)
  = foldr (||) False (map (\(Cell (_, y) _) -> y >= height-1) cells)

-- | Tetromino factory based on its type
getTetromino :: Type -> Tetromino
getTetromino I = (Tetromino (5, 23) [(RelativeCell pos (light blue)) | pos <- [(-1, 0), (0, 0), (1, 0), (2, 0)]])
getTetromino J = (Tetromino (5, 23) [(RelativeCell pos blue) | pos <- [(-1, 0), (0, 0), (1, 0), (-1, 1)]])
getTetromino L = (Tetromino (5, 23) [(RelativeCell pos orange) | pos <- [(-1, 0), (0, 0), (1, 0), (1, 1)]])
getTetromino O = (Tetromino (5, 23) [(RelativeCell (x, y) yellow) | x <- [0,1], y <- [0,1]])
getTetromino S = (Tetromino (5, 23) [(RelativeCell pos green) | pos <- [(-1, 0), (0, 0), (1, 1), (0, 1)]])
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
    relPositions = map (\(RelativeCell p _) -> p) relCells

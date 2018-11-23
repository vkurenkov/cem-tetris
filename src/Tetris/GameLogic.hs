{-# LANGUAGE OverloadedStrings #-}
module Tetris.GameLogic where

import CodeWorld
import Tetris.GameTypes
import System.Random
import Data.Random.Normal

fallOffset :: Offset
fallOffset = (0, -1)

-- | Init general game state function.
-- | It is function that contains gameStates for bot User and Bot
initGeneralGameState :: StdGen -> GeneralGameState
initGeneralGameState gen = GeneralGameState (initGameState gen) (initGameState gen)

-- | Init game state function.
-- | It is the same for User and Bot
initGameState :: StdGen -> GameState
initGameState gen = GameState (Field 22 10) Nothing [] gen 0

-- | Updates current general game state
updateGeneralGameState :: GeneralGameState -> GeneralGameState
updateGeneralGameState (GeneralGameState userGs botGs)
  = GeneralGameState (updateGameState userGs) (updateGameState botGs)

-- | Updates current game state
-- | It is the same for User and Bot
updateGameState :: GameState -> GameState
updateGameState gs
  | isFinished gs = gs
  | otherwise = (removeFilledRows
               . fallTetromino
               . handleCollision
               . generateTetromino) gs

handleGeneralGameState :: Event -> GeneralGameState -> GeneralGameState
handleGeneralGameState event ggs
  | event == KeyPress "L" = GeneralGameState (initGameState uGen) botGs
  | event == KeyPress "R" = GeneralGameState userGs (initGameState bGen)
  | event == KeyPress "A" = GeneralGameState (initGameState uGen) (initGameState uGen)
  | otherwise
    = GeneralGameState
      (handleUserGameState event userGs)
      (handleBotGameState event botGs)
  where
    (GeneralGameState userGs botGs) = ggs
    (GameState _ _ _ uGen _)        = userGs
    (GameState _ _ _ bGen _)        = botGs

-- | Handles user input events
handleUserGameState :: Event -> GameState -> GameState
handleUserGameState event gameState = applyGameAction action gameState
  where
    action
      | event == (KeyPress "Up") = Just Rotate
      | event == (KeyPress "Down") = Just MoveDown
      | event == (KeyPress "Left") = Just MoveLeft
      | event == (KeyPress "Right") = Just MoveRight
      | otherwise = Nothing

-- | Handles bot input events
-- | (Our bot doesn't simulate CodeWorld events)
handleBotGameState :: Event -> GameState -> GameState
handleBotGameState _ = id

-- | Applies given game action
applyGameAction :: Maybe GameAction -> GameState -> GameState
applyGameAction Nothing gameState = gameState
applyGameAction _ (GameState field Nothing cells gen score) = GameState field Nothing cells gen score
applyGameAction (Just MoveDown) gameState = updateGameState gameState
applyGameAction (Just action) (GameState field (Just tetromino) cells gen score)
  = GameState field newTetromino cells gen score
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

-- | Offsets tetromino by given offset
offsetTetromino :: Offset -> Tetromino -> Tetromino
offsetTetromino (dx, dy) (Tetromino (x, y) rCells)
  = Tetromino (x + dx, y + dy) rCells

-- | Game update for falling tetromino
fallTetromino :: GameState -> GameState
fallTetromino (GameState field Nothing cells gen score)
  = (GameState field Nothing cells gen score)
fallTetromino (GameState field (Just tetromino) cells gen score)
  = (GameState field offsetedTetromino cells gen score)
  where
    offsetedTetromino = Just (offsetTetromino fallOffset tetromino)

-- | Check position of tetromino, if touches objects or bottom line add to cells
handleCollision :: GameState -> GameState
handleCollision (GameState field Nothing cells gen score)
  = GameState field Nothing cells gen score
handleCollision (GameState field (Just tetromino) cells gen score)
  | touchesBottom = GameState field Nothing newCells gen score
  | touchesCells = GameState field Nothing newCells gen score
  | otherwise = GameState field (Just tetromino) cells gen score
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
removeFilledRows (GameState field tetromino cells gen score)
  = (GameState field tetromino newCells gen newScore)
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
generateTetromino gs = gs

intToTetromino :: Integer -> Maybe Tetromino
intToTetromino 0 = Just (getTetromino Z)
intToTetromino 1 = Just (getTetromino L)
intToTetromino 2 = Just (getTetromino O)
intToTetromino 3 = Just (getTetromino S)
intToTetromino 4 = Just (getTetromino I)
intToTetromino 5 = Just (getTetromino J)
intToTetromino 6 = Just (getTetromino T)
intToTetromino _ = Nothing

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

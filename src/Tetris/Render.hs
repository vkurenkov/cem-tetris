module Tetris.Render where

import Tetris.GameTypes
import CodeWorld

backgroundColor :: Color
backgroundColor = black


-- | Renders game state: field, falling tetromino, and left cells
renderGameState :: GameState -> Picture
renderGameState (GameState field tetromino cells)
  = scaled 0.7 0.7 
     (renderTetromino tetromino
      <> renderCells cells
      <> renderField backgroundColor field)
    where
      renderCells cs = mconcat (map renderCell cs)

-- | Renders tetromino if there is any
renderTetromino :: Maybe Tetromino -> Picture
renderTetromino Nothing
  = blank
renderTetromino (Just (Tetromino pos rCells))
  = mconcat (map renderCell (map (relativeToAbs pos) rCells))
  
-- | Renders background field with specified color
renderField :: Color -> Field -> Picture
renderField bgC (Field h w)
  = mconcat (map renderCell grid)
    where
      grid = [Cell (x, y) bgC | x <- [0..(w-1)], y <- [0..(h-1)]]

-- | Renders particular left cell
renderCell :: Cell  -> Picture
renderCell (Cell (x, y) c) = colored c (translated i j (solidRectangle 1.0 1.0))
   where
    (i, j) = intsToDoubles (x, y)


relativeToAbs :: Position -> RelativeCell -> Cell
relativeToAbs (x, y) (RelativeCell (dx, dy) color)
  = Cell (x + dx, y + dy) color

intsToDoubles:: (Integer, Integer) -> (Double, Double)
intsToDoubles (x, y) = (fromInteger x, fromInteger y)
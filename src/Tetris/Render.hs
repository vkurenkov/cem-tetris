{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Tetris.Render where

import Tetris.GameTypes
import Tetris.GameLogic
import CodeWorld
import Data.String
import System.Random


backgroundColor :: Color
backgroundColor = black


-- | Renders game state: field, falling tetromino, and left cells
renderGameState :: GameState -> Picture
renderGameState (GameState field tetromino cells gen score)
  = scaled 0.7 0.7 (translated (-10.0) (-10.0) rendered)
    where
      rendered
        =  renderTetromino tetromino
        -- <> coordinatePlane
        -- <> translated 0 20 coordinatePlane
        -- <> translated 20 0 coordinatePlane
        -- <> translated 20 20 coordinatePlane
        <> translated 13 17 (renderText "Next:")
        <> translated 10.5 3 smallTetromino
        <> translated 13 15 smallBox
        <> renderCells cells
        <> renderField backgroundColor field
        <> translated (w + 3) (h - 2) (renderScore score)
        <> translated (-1) (-1) (renderDecorations 18 24)

      smallTetromino = translated (-0.25) 0 (scaled 0.5 0.5 (renderTetromino nextTetromino))
      smallBox
        = colored (grey 0.9) (solidRectangle 3.5 2)
        <> thickRectangle 0.2 3.5 2

      Field height width = field
      w = fromInteger width
      h = fromInteger height
      (nNumber, _) = randomR (0,6) gen
      nextTetromino = intToTetromino nNumber

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

-- | Render scoring
renderScore :: Integer -> Picture
renderScore n = (lettering . fromString) ("Score: " ++ show n)

-- | Render text
renderText :: String -> Picture
renderText = lettering . fromString

-- | Render decorations in upper-right direction
-- | w = 18, h = 24
renderDecorations :: Double -> Double -> Picture
renderDecorations w h
  = translated (w/2 - 0.5) (h/2 - 0.5) (mBack <> mBorder)
  where
    mBack   = colored (grey 0.85) (solidRectangle w h)
    mBorder = colored black (thickRectangle 0.2 w h)

relativeToAbs :: Position -> RelativeCell -> Cell
relativeToAbs (x, y) (RelativeCell (dx, dy) color)
  = Cell (x + dx, y + dy) color

intsToDoubles:: (Integer, Integer) -> (Double, Double)
intsToDoubles (x, y) = (fromInteger x, fromInteger y)

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

renderGeneralGameState :: GeneralGameState -> Picture
renderGeneralGameState (GeneralGameState userGs botGs)
  = renderUserGameState userGs
  <> renderBotGameState botGs

-- | Rendes User's game
renderUserGameState :: GameState -> Picture
renderUserGameState gs
  = translated (-12.0) (-7.0) (renderGameState "User" gs)

-- | Rendes Bot's game
renderBotGameState :: GameState -> Picture
renderBotGameState gs
  = translated 1 (-7.0) (renderGameState "Bot" gs)

-- | Renders game state: field, falling tetromino, and left cells
renderGameState :: String -> GameState -> Picture
renderGameState title (GameState field tetromino cells gen score)
  = scaled 0.7 0.7 rendered
    where
      rendered
        =  renderTetromino tetromino
        -- <> coordinatePlane
        -- <> translated 0 20 coordinatePlane
        -- <> translated 20 0 coordinatePlane
        -- <> translated 20 20 coordinatePlane
        --
        -- <> translated 13 17 (renderText "Next:")
        -- <> translated 10.5 3 smallTetromino
        -- <> translated 13 15 smallBox
        <> translated (w + 3) (h - 1) (renderText title)
        <> translated (w + 3) (h - 3.5) (renderText "Score")
        <> translated (w + 3) (h - 4.5) (renderScore score)
        <> translated (w + 3) (h - 7) (renderText "Next:")
        <> translated 10.5 1 smallTetromino
        <> translated 13 13 smallBox
        <> renderCells cells
        <> renderField backgroundColor field
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
renderScore = lettering . fromString . show

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

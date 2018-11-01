module Tetris.GameTypes where

import CodeWorld

-- | Position in the game world
type Position = (Integer, Integer)
-- | Can be treated as velocity
type Offset = (Integer, Integer)

-- | Game field
data Field = Field Height Width
type Height = Integer
type Width = Integer

-- | As defined in http://tetris.wikia.com/wiki/Tetromino
data Type = Z | L | O | S | I | J | T
-- | Block of cells (basic game component)
data Tetromino = Tetromino Position [RelativeCell]
-- | Used to describe relative cells of the tetromino
data RelativeCell = RelativeCell Position Color

-- | Game cell that is already not a part of the tetromino
data Cell = Cell Position Color

-- | Describes current state of the game
data GameState = GameState Field (Maybe Tetromino) [Cell]
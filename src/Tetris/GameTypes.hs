module Tetris.GameTypes where

import CodeWorld
import System.Random

-- | Position in the game world
type Position = (Integer, Integer)
-- | Can be treated as velocity
type Offset = (Integer, Integer)

-- | Game field
data Field       = Field Height Width
type Height      = Integer
type Width       = Integer
type Score       = Integer

-- | As defined in http://tetris.wikia.com/wiki/Tetromino
data Type = Z | L | O | S | I | J | T

-- | Block of cells (basic game component)
data Tetromino = Tetromino Position [RelativeCell]
-- | Used to describe relative cells of the tetromino
data RelativeCell = RelativeCell Position Color

-- | Game cell that is already not a part of the tetromino
data Cell = Cell Position Color

-- | Describes current state of the game
-- | Last integer is for random generation
data GameState = GameState Field (Maybe Tetromino) [Cell] StdGen Score

-- | General State 
-- | Combines game state for User and for Bot
data GeneralGameState = GeneralGameState GameState GameState

-- | Game actions
data GameAction = Rotate | MoveDown | MoveLeft | MoveRight deriving (Eq)

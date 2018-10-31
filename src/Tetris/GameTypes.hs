module Tetris.GameTypes where

import CodeWorld

type Height = Integer
type Width = Integer

type Position = (Integer, Integer)
type Velocity = (Integer, Integer)

data Cell = Cell Position Color
data RelativeCell = RelativeCell Position Color

data Field = Field Height Width
data Tetromino = Tetromino Position [RelativeCell]

data GameState = GameState Field (Maybe Tetromino) [Cell]

data Type = O
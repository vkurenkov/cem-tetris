module Tetris.BotTypes where

import Tetris.GameTypes

-- | Agent is represented by a list of weights
data Agent = Agent [Float] deriving (Show, Eq)

-- | An intermediate representation of the training: holds distribution means and stds
data Training = Training [Float] [Float] deriving (Show)

-- | Mean score
type MeanScore = Rational

-- | The result of the training: trained agent and mean score for every training iteration
data TrainingResult = TrainingResult Agent [MeanScore] deriving (Show)

-- | Describes an episode
data Episode = Episode Agent Score deriving (Eq, Show)

instance Ord Episode where
  compare (Episode _ score) (Episode _ score1) = compare score score1

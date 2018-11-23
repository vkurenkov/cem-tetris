module CrossEntropyMethod where

import Tetris.GameTypes
import Data.List
import Data.Ord
import System.Random
import Data.Random.Normal

-- | Agent is represented by a list of weights
data Agent = Agent [Float] deriving (Show, Eq)

-- | An intermediate representation of the training: holds distribution means and stds
data Training = Training [Float] [Float] deriving (Show)

-- | Mean score
type MeanScore = Rational

-- | The result of the training: trained agent and mean score for every training iteration
data TrainingResult = TrainingResult Agent [MeanScore]

-- | Describes an episode
data Episode = Episode Agent Score deriving (Eq)

instance Ord Episode where
  compare (Episode _ score) (Episode _ score1) = compare score score1


average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

-- | Number of weights in the value function (i.e. number of basis functions)
numWeights :: Int
numWeights = 22


-- | Returns maximum height of the columns
maxColumnHeight :: [Cell] -> Integer
maxColumnHeight cells
  | length cells == 0 = 0
  | otherwise = (maximum (map (\(Cell (_, y) _) -> y) cells)) + 1


-- | Returns height of every column
columnHeights :: Field -> [Cell] -> [Integer]
columnHeights (Field _ width) cells 
  = [columnHeight i | i <- [0..width-1]]
  where
    columnHeight ind 
      | length (filterByWidth ind cellPositions) == 0 = 0
      | otherwise = (maximum (map (\(_, y) -> y) (filterByWidth ind cellPositions))) + 1
    filterByWidth ind positions = filter (\(x, _) -> x == ind) positions
    cellPositions = map (\(Cell (x, y) _) -> (x, y)) cells


-- | Returns differences in column heights
diffColumnHeights :: Field -> [Cell] -> [Integer]
diffColumnHeights field cells = differences (columnHeights field cells)
  where
    differences heights
      | length heights == 1 = []
      | otherwise = [abs ((head heights) - (heights !! 1))] ++ (differences (tail heights))

-- | Number of holes
numHoles :: Field -> [Cell] -> Integer
numHoles (Field height width) cells = totNumCells - numFilledCells - numReachedCells + width
  where
    totNumCells = height * width
    numFilledCells = genericLength cells
    numReachedCells = genericLength (visitCells 0 (height) [])
    visitCells curWidth curHeight visited
      | curHeight < 0 = []
      | curWidth < 0  = []
      | curWidth >= width = []
      | isWall = []
      | alreadyVisited = []
      | otherwise = [(curWidth, curHeight)] ++ downReached ++ leftReached ++ rightReached
      where
        isWall         = elem (curWidth, curHeight) (map (\(Cell (x_, y_) _) -> (x_, y_)) cells)
        alreadyVisited = elem (curWidth, curHeight) visited
        newVisited     = visited ++ [(curWidth, curHeight)]

        downReached  = visitCells curWidth (curHeight - 1) newVisited
        leftReached  = visitCells (curWidth - 1) curHeight (newVisited ++ downReached)
        rightReached = visitCells (curWidth + 1) curHeight (newVisited ++ downReached ++ leftReached)

-- | Assess the value of the game state
stateValue :: GameState -> [Rational] -> Rational
stateValue (GameState field _ cells _ _) weights
  = bias + weightedMaxColumnHeight + weightedColumnHeights + weightedDiffColumnHeights + weightedNumHoles
  where
    bias = weights !! 0
    weightedMaxColumnHeight = (weights !! 1) * toRational (maxColumnHeight cells)
    weightedColumnHeights = sum [(weights !! (i + 2)) * toRational ((columnHeights field cells) !! i) | i <- [0..(length cells) - 1]]
    weightedDiffColumnHeights = sum [(weights !! (i + 2 + (length cells))) * toRational ((columnHeights field cells) !! i) | i <- [0..(length cells) - 2]]
    weightedNumHoles = (last weights) * toRational (numHoles field cells)


-- | Creates an initial training
initTraining :: Training
initTraining = Training initMeans initStds
  where
    initMeans = [0.0 | _ <- [1..numWeights]]    -- | As defined in Learning Tetris Using Noisy Cross-Entropy
    initStds  = [100.0 | _ <- [1..numWeights]]  -- | As defined in Learning Tetris Using Noisy Cross-Entropy


-- | Sample an agent
sampleAgent 
  :: StdGen 
  -> [Float] 
  -> [Float] 
  -> (Agent, StdGen)
sampleAgent startGen means stds = ((Agent weights), lastGen)
  where
    (weights, lastGen) = sample startGen numWeights
    sample :: StdGen -> Int -> ([Float], StdGen)
    sample curGen ind
      | ind <= 0 = ([], curGen)
      | otherwise = ([weight] ++ weights_, lastGen_)
      where
        (weights_, lastGen_) = sample newGen (ind - 1)
        (weight, newGen) = normal' (means !! (ind - 1), stds !! (ind - 1)) curGen


-- | Sample agents
sampleAgents
  :: Integer    -- | Num of agents to sample
  -> StdGen  -- | Starting random generator
  -> [Float] -- | Means
  -> [Float] -- | Stds
  -> ([Agent], StdGen)
sampleAgents numAgents startGen means stds
  = sample numAgents startGen
  where
    sample agentsLeft curGen
      | agentsLeft <= 0 = ([], curGen)
      | otherwise = ([agent] ++ agents, lastGen)
      where
        (agents, lastGen) = sample (agentsLeft - 1) newGen
        (agent, newGen) = sampleAgent curGen means stds


-- | A learning process
trainAgent 
  :: StdGen     -- Starting generator
  -> Integer    -- Number of game samples per step
  -> Rational   -- Selection ratio
  -> Integer    -- Maximum number of iteration steps
  -> (TrainingResult, StdGen)
trainAgent startGen numGameSamples selectionRatio maxTrainSteps 
  = trainStep startGen 0 initTraining
  where
    trainStep :: StdGen -> Integer -> Training -> (TrainingResult, StdGen)
    trainStep curGen curStep (Training curMeans curStds)
      | curStep >= maxTrainSteps = (TrainingResult (Agent curMeans) [], curGen)
      | otherwise = (TrainingResult agent ([meanScore] ++ meanScores), lastGen)
          where
            (agents, gen)                              = sampleAgents numGameSamples curGen curMeans curStds
            (episodes, gen1)                           = runEpisodes gen agents
            scores                                     = map (\(Episode _ score) -> score) episodes
            meanScore                                  = average scores
            selectedAgents                             = selectTopAgents episodes
            updatedTraining                            = updateTraining selectedAgents
            (TrainingResult agent meanScores, lastGen) = trainStep gen1 (curStep + 1) updatedTraining

            runEpisodes :: StdGen -> [Agent] -> ([Episode], StdGen)
            runEpisodes = _

            selectTopAgents :: [Episode] -> [Agent]
            selectTopAgents episodes = take numToSelect (extractAgents (reverse (sort episodes)))
              where
                extractAgents eps = map (\(Episode agent score) -> agent) eps
                numToSelect       = floor (selectionRatio * (genericLength episodes))

            updateTraining :: [Agent] -> Training
            updateTraining agents = Training weightsMeans weightsStds
              where
                numAgents    = genericLength agents
                weights      = transpose (map (\(Agent weights_) -> weights_) agents)
                weightsMeans = map (\(ws) -> average ws) weights
                weightsStds  = map (\(ws, m) -> (foldl (\s w -> (s + (w - m) * (w - m))) 0.0 ws) / numAgents) (zip weights weightsMeans)
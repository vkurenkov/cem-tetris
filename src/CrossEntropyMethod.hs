module CrossEntropyMethod where

import Tetris.GameTypes
import Tetris.BotTypes
import Tetris.GameLogic
import Data.List
import Data.Ord
import System.Random
import Data.Random.Normal

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

-- | Number of weights in the value function (i.e. number of basis functions)
numWeights :: Int
numWeights = 22

-- | Creates an initial training
initTraining :: Training
initTraining = Training initMeans initStds
  where
    initMeans = [-10.0 | _ <- [1..numWeights]]    -- | Smart Initialization
    initStds  = [10.0 | _ <- [1..numWeights]]     -- | As defined in Learning Tetris Using Noisy Cross-Entropy

-- | Sample an agent
sampleAgent
  :: StdGen
  -> [Float]
  -> [Float]
  -> (Agent, StdGen)
sampleAgent startGen means stds = (Agent weights, lastGen)
  where
    (weights, lastGen) = sample startGen 0
    sample :: StdGen -> Int -> ([Float], StdGen)
    sample curGen ind
      | ind >= numWeights = ([], curGen)
      | otherwise = (weight : weights_, lastGen_)
      where
        (weights_, lastGen_) = sample newGen (ind + 1)
        (weight, newGen) = normal' (means !! ind, stds !! ind) curGen

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
      | otherwise = (agent : agents, lastGen)
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
      | otherwise = (TrainingResult agent (meanScore : meanScores), lastGen)
          where
            (agents, gen)                              = sampleAgents numGameSamples curGen curMeans curStds
            (episodes, gen1)                           = runEpisodes gen agents
            scores                                     = map (\(Episode _ score) -> score) episodes
            meanScore                                  = average scores
            selectedAgents                             = selectTopAgents episodes
            updatedTraining                            = updateTraining selectedAgents
            (TrainingResult agent meanScores, lastGen) = trainStep gen1 (curStep + 1) updatedTraining

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
    weightsMeans = map average weights
    weightsStds  = map (\(ws, m) -> sqrt ((foldl (\s w -> (s + (w - m) * (w - m))) 0.0 ws) / numAgents)) (zip weights weightsMeans)

-- -- | Run episodes for every given agent
runEpisodes
  :: StdGen   -- Initial random generator
  -> [Agent]  -- Agents to be tested
  -> ([Episode], StdGen)
runEpisodes curGen [] = ([], curGen)
runEpisodes curGen (agent:agents)
  = (episode : episodes, lastGen)
    where
      (episode, newGen) = runEpisode curGen agent
      (episodes, lastGen) = runEpisodes newGen agents

-- | Run one episode
runEpisode :: StdGen -> Agent -> (Episode, StdGen)
runEpisode startGen agent = ((Episode agent score), newGen)
  where
    gameState = initGameState startGen
    (score, newGen) = simulate agent gameState

-- | Simulate complete game until isFinished
simulate :: Agent -> GameState -> (Score, StdGen)
simulate agent curGameState
  | isFinished curGameState = (score, curGen)
  | not (justGenerated field tetromino) = simulate agent (updateGameState curGameState)
  | otherwise = simulate agent (updateGameState newGameState)
      where
        (GameState field tetromino cells curGen score) = curGameState
        newGameState = GameState field (Just newTetromino) cells curGen score
        newTetromino = getBestTetromino agent curGameState

-- | Training process
run :: IO ()
run = do
  let g = mkStdGen 42
  let (result, _) = trainAgent g 100 0.1 10

  print result

module CrossEntropyMethod where

import Tetris.GameTypes
import Data.List
import System.Random


-- | Agent is represented by a list of weights
data Agent = Agent [Rational]

-- | Mean score
type MeanScore = Rational

-- | The result of the training: trained agent and mean score for every training iteration
data TrainingResult = TrainingResult Agent [MeanScore]


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

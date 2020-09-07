module Maze
  ( EdgeStateMap
  , EdgeState(..)
  , Cell
  , Direction(..)
  , Maze
  , MazeSize(..)
  , getEdge
  , getEdgeState
  , generateMaze
  ) where

import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import qualified Data.Set        as S
import           System.Random   (randomRIO)

-- Types ---------------------------------------------------------------------------

type Cell = (Int, Int)

              -- Top  Bottom
              -- Left Right
data Edge = Edge Cell Cell deriving (Eq, Ord, Show)

data EdgeState = Open | Wall deriving (Eq, Show)

type EdgeStateMap = M.Map Edge EdgeState

data Direction = TopDir | RightDir | BottomDir | LeftDir deriving (Eq, Enum, Bounded)

data MazeSize = Small | Medium | Large deriving (Bounded, Enum, Eq, Read, Show)

            --rows --cols
type Maze = ([Int], [Int], EdgeStateMap)

-- Utils ---------------------------------------------------------------------------

getNeighbor :: Direction -> Cell -> Cell
getNeighbor TopDir (r, c)    = (r - 1, c)
getNeighbor RightDir (r, c)  = (r, c + 1)
getNeighbor BottomDir (r, c) = (r + 1, c)
getNeighbor LeftDir (r, c)   = (r, c - 1)

getEdge :: Direction -> Cell -> Edge
getEdge TopDir cell    = Edge (getNeighbor TopDir cell) cell
getEdge RightDir cell  = Edge cell (getNeighbor RightDir cell)
getEdge BottomDir cell = Edge cell (getNeighbor BottomDir cell)
getEdge LeftDir cell   = Edge (getNeighbor LeftDir cell) cell

getEdgeState :: EdgeStateMap -> Edge -> EdgeState
getEdgeState edgeStateMap edge = fromMaybe Wall . M.lookup edge $ edgeStateMap

isCellValid :: S.Set Cell -> Cell -> Bool
isCellValid cellSet cell = S.member cell cellSet

isEdgeValid :: S.Set Cell -> Edge -> Bool
isEdgeValid cellSet (Edge cell1 cell2) =
  isCellValid cellSet cell1 && isCellValid cellSet cell2

isUnvisited :: S.Set Cell -> Cell -> Bool
isUnvisited visited neighbor = not . S.member neighbor $ visited

getRandomDir :: [Direction] -> IO (Maybe Direction, [Direction])
getRandomDir [] = return (Nothing, [])
getRandomDir dirs = do
  i <- randomRIO (0, length dirs - 1)
  let dir = dirs !! i
      dirs' = filter (/= dir) dirs
  return $ (Just dir, dirs')

-- Maze ----------------------------------------------------------------------------

getInitialEdgeStateMap :: [Cell] -> S.Set Cell -> EdgeStateMap
getInitialEdgeStateMap cells cellSet = M.fromList . map (flip (,) $ Wall) $ edges
  where edges = filter (isEdgeValid cellSet) $
          pure getEdge <*> [ minBound .. maxBound ] <*> cells

visitNeighbor ::
  S.Set Cell ->
  EdgeStateMap ->
  S.Set Cell ->
  Direction ->
  Cell ->
  IO (EdgeStateMap, S.Set Cell)
visitNeighbor cellSet edgeStateMap visited dir cell = do
  let neighbor = getNeighbor dir cell
  if isCellValid cellSet neighbor && isUnvisited visited neighbor
    then do
      let edgeStateMap' = M.insert (getEdge dir cell) Open edgeStateMap
      visitCell cellSet edgeStateMap' visited neighbor
    else return (edgeStateMap, visited)

visitNeighbors ::
  S.Set Cell ->
  EdgeStateMap ->
  S.Set Cell ->
  [Direction] ->
  Cell ->
  IO (EdgeStateMap, S.Set Cell)
visitNeighbors cellSet edgeStateMap visited dirs cell = do
  (maybeDir, dirs') <- getRandomDir dirs
  case maybeDir of
    Nothing -> return (edgeStateMap, visited)
    Just dir -> do
      (edgeStateMap', visited') <- visitNeighbor cellSet edgeStateMap visited dir cell
      visitNeighbors cellSet edgeStateMap' visited' dirs' cell

visitCell ::
  S.Set Cell ->
  EdgeStateMap ->
  S.Set Cell ->
  Cell ->
  IO (EdgeStateMap, S.Set Cell)
visitCell cellSet edgeStateMap visited cell = do
  let visited' = S.insert cell visited
  visitNeighbors cellSet edgeStateMap visited' [minBound .. maxBound] cell

getCells :: ([Int], [Int]) -> [Cell]
getCells (rows, cols) = do
  r <- rows
  c <- cols
  return (r, c)

getMazeDimensions :: MazeSize -> ([Int], [Int])
getMazeDimensions mazeSize = ([0..rowCount], [0..colCount])
  where (rowCount, colCount) = case mazeSize of
                     Small  -> (10, 5)
                     Medium -> (15, 10)
                     Large  -> (20, 15)

generateMaze :: MazeSize -> IO ([Int], [Int], EdgeStateMap)
generateMaze mazeSize = do
  let (rows, cols) = getMazeDimensions mazeSize
      cells = getCells (rows, cols)
      cellSet = S.fromList cells
      initialEdgeStateMap = getInitialEdgeStateMap cells cellSet
  (edgeStateMap, _) <- visitCell cellSet initialEdgeStateMap S.empty (head cells)
  return (rows, cols, edgeStateMap)


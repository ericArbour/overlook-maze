module Maze
  ( EdgeStateMap
  , EdgeState(..)
  , Cell
  , Direction(..)
  , getEdge
  , getEdgeState
  , cols
  , rows
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

-- Utils ---------------------------------------------------------------------------

getNeighbor :: Direction -> Cell -> Cell
getNeighbor TopDir (r, c) = (r - 1, c)
getNeighbor RightDir (r, c) = (r, c + 1)
getNeighbor BottomDir (r, c) = (r + 1, c)
getNeighbor LeftDir (r, c) = (r, c - 1)

getEdge :: Direction -> Cell -> Edge
getEdge TopDir cell = Edge (getNeighbor TopDir cell) cell
getEdge RightDir cell = Edge cell (getNeighbor RightDir cell)
getEdge BottomDir cell = Edge cell (getNeighbor BottomDir cell)
getEdge LeftDir cell = Edge (getNeighbor LeftDir cell) cell

getEdgeState :: EdgeStateMap -> Edge -> EdgeState
getEdgeState edgeStateMap edge = fromMaybe Wall . M.lookup edge $ edgeStateMap

rows :: [Int]
rows = [0..20]

cols :: [Int]
cols = [0..13]

cells :: [Cell]
cells = do
  r <- rows
  c <- cols
  return (r, c)

cellSet :: S.Set Cell
cellSet = S.fromList cells

isCellValid :: Cell -> Bool
isCellValid cell = S.member cell cellSet

isEdgeValid :: Edge -> Bool
isEdgeValid (Edge cell1 cell2) = isCellValid cell1 && isCellValid cell2

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

initialEdgeStateMap :: EdgeStateMap
initialEdgeStateMap = M.fromList . map (flip (,) $ Wall) $ edges
  where edges = filter isEdgeValid $
          pure getEdge <*> [ minBound .. maxBound ] <*> cells

visitNeighbor ::
  EdgeStateMap ->
  S.Set Cell ->
  Direction ->
  Cell ->
  IO (EdgeStateMap, S.Set Cell)
visitNeighbor edgeStateMap visited dir cell = do
  let neighbor = getNeighbor dir cell
  if isCellValid neighbor && isUnvisited visited neighbor
    then do
      let edgeStateMap' = M.insert (getEdge dir cell) Open edgeStateMap
      visitCell edgeStateMap' visited neighbor
    else return (edgeStateMap, visited)

visitNeighbors ::
  EdgeStateMap ->
  S.Set Cell ->
  [Direction] ->
  Cell ->
  IO (EdgeStateMap, S.Set Cell)
visitNeighbors edgeStateMap visited dirs cell = do
  (maybeDir, dirs') <- getRandomDir dirs
  case maybeDir of
    Nothing -> return (edgeStateMap, visited)
    Just dir -> do
      (edgeStateMap', visited') <- visitNeighbor edgeStateMap visited dir cell
      visitNeighbors edgeStateMap' visited' dirs' cell

visitCell :: EdgeStateMap -> S.Set Cell -> Cell -> IO (EdgeStateMap, S.Set Cell)
visitCell edgeStateMap visited cell = do
  let visited' = S.insert cell visited
  visitNeighbors edgeStateMap visited' [minBound .. maxBound] cell

generateMaze :: IO EdgeStateMap
generateMaze = do
  (edgeStateMap, _) <- visitCell initialEdgeStateMap S.empty (head cells)
  return edgeStateMap


module Maze
  ( EdgeStateMap
  , EdgeState(..)
  , Cell
  , edgeState
  , topEdge
  , rightEdge
  , bottomEdge
  , leftEdge
  , cols
  , rows
  , generateMaze
  ) where

import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import qualified Data.Set        as S
import           System.Random   (randomRIO)

type Cell = (Int, Int)

              -- Top  Bottom
              -- Left Right
data Edge = Edge Cell Cell deriving (Eq, Ord, Show)

data EdgeState = Open | Wall deriving (Eq, Show)

type EdgeStateMap = M.Map Edge EdgeState

topNeighbor :: Cell -> Cell
topNeighbor (r, c) = (r - 1, c)

rightNeighbor :: Cell -> Cell
rightNeighbor (r, c) = (r, c + 1)

bottomNeighbor :: Cell -> Cell
bottomNeighbor (r, c) = (r + 1, c)

leftNeighbor :: Cell -> Cell
leftNeighbor (r, c) = (r, c - 1)

topEdge :: Cell -> Edge
topEdge cell = Edge (topNeighbor cell) cell

rightEdge :: Cell -> Edge
rightEdge cell = Edge cell (rightNeighbor cell)

bottomEdge :: Cell -> Edge
bottomEdge cell = Edge cell (bottomNeighbor cell)

leftEdge :: Cell -> Edge
leftEdge cell = Edge (leftNeighbor cell) cell

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

initialEdgeStateMap :: EdgeStateMap
initialEdgeStateMap = M.fromList . map (flip (,) $ Wall) $ edges
  where edges = filter isEdgeValid $
          [ topEdge
          , rightEdge
          , bottomEdge
          , leftEdge
          ] <*> cells

edgeState :: EdgeStateMap -> Edge -> EdgeState
edgeState edgeStateMap edge = fromMaybe Wall . M.lookup edge $ edgeStateMap

data Direction = TopDir | RightDir | BottomDir | LeftDir deriving (Eq, Enum, Bounded)

getDirFns :: Direction -> ((Cell -> Cell), (Cell -> Edge))
getDirFns TopDir    = (topNeighbor, topEdge)
getDirFns RightDir  = (rightNeighbor, rightEdge)
getDirFns BottomDir = (bottomNeighbor, bottomEdge)
getDirFns LeftDir   = (leftNeighbor, leftEdge)

isUnvisited :: S.Set Cell -> Cell -> Bool
isUnvisited visited neighbor = not . S.member neighbor $ visited

getRandomDir :: [Direction] -> IO (Maybe Direction, [Direction])
getRandomDir [] = return (Nothing, [])
getRandomDir dirs = do
  i <- randomRIO (0, length dirs - 1)
  let dir = dirs !! i
      dirs' = filter (/= dir) dirs
  return $ (Just dir, dirs')

visitNeighbor ::
  EdgeStateMap ->
  S.Set Cell ->
  Cell ->
  Direction ->
  IO (EdgeStateMap, S.Set Cell)
visitNeighbor edgeStateMap visited cell dir = do
  let (neighborFn, edgeFn) = getDirFns dir
      neighbor = neighborFn cell
  if isCellValid neighbor && isUnvisited visited neighbor
    then do
      let edgeStateMap' = M.insert (edgeFn cell) Open edgeStateMap
      visitCell edgeStateMap' visited neighbor
    else return (edgeStateMap, visited)

visitNeighbors ::
  EdgeStateMap ->
  S.Set Cell ->
  Cell ->
  [Direction] ->
  IO (EdgeStateMap, S.Set Cell)
visitNeighbors edgeStateMap visited cell dirs = do
  (maybeDir, dirs') <- getRandomDir dirs
  case maybeDir of
    Nothing -> return (edgeStateMap, visited)
    Just dir -> do
      (edgeStateMap', visited') <- visitNeighbor edgeStateMap visited cell dir
      visitNeighbors edgeStateMap' visited' cell dirs'

visitCell :: EdgeStateMap -> S.Set Cell -> Cell -> IO (EdgeStateMap, S.Set Cell)
visitCell edgeStateMap visited cell = do
  let visited' = S.insert cell visited
  visitNeighbors edgeStateMap visited' cell [minBound .. maxBound]

generateMaze :: IO EdgeStateMap
generateMaze = do
  (edgeStateMap, _) <- visitCell initialEdgeStateMap S.empty (head cells)
  return edgeStateMap


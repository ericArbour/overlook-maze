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

newtype ValidCells = ValidCells { getValidCells :: S.Set Cell }

newtype VisitedCells = VisitedCells { getVisitedCells :: S.Set Cell }

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
getEdgeState edgeStateMap edge = fromMaybe Wall $ M.lookup edge edgeStateMap

isCellValid :: ValidCells -> Cell -> Bool
isCellValid validCells cell = S.member cell (getValidCells validCells)

isEdgeValid :: ValidCells -> Edge -> Bool
isEdgeValid validCells (Edge cell1 cell2) =
  isCellValid validCells cell1 && isCellValid validCells cell2

isUnvisited :: VisitedCells -> Cell -> Bool
isUnvisited visitedCells neighbor =
  not . S.member neighbor $ getVisitedCells visitedCells

getRandomDir :: [Direction] -> IO (Maybe Direction, [Direction])
getRandomDir [] = return (Nothing, [])
getRandomDir dirs = do
  i <- randomRIO (0, length dirs - 1)
  let dir = dirs !! i
      dirs' = filter (/= dir) dirs
  return $ (Just dir, dirs')

-- Maze ----------------------------------------------------------------------------

getInitialEdgeStateMap :: [Cell] -> ValidCells -> EdgeStateMap
getInitialEdgeStateMap cells validCells = M.fromList . map (flip (,) $ Wall) $ edges
  where edges = filter (isEdgeValid validCells) $
          getEdge <$> [ minBound .. maxBound ] <*> cells

visitNeighbor ::
  ValidCells ->
  VisitedCells ->
  EdgeStateMap ->
  Direction ->
  Cell ->
  IO (EdgeStateMap, VisitedCells)
visitNeighbor validCells visitedCells  edgeStateMap dir cell = do
  let neighbor = getNeighbor dir cell
  if isCellValid validCells neighbor && isUnvisited visitedCells neighbor
    then do
      let edgeStateMap' = M.insert (getEdge dir cell) Open edgeStateMap
      visitCell validCells visitedCells edgeStateMap' neighbor
    else return (edgeStateMap, visitedCells)

visitNeighbors ::
  ValidCells ->
  VisitedCells ->
  EdgeStateMap ->
  [Direction] ->
  Cell ->
  IO (EdgeStateMap, VisitedCells)
visitNeighbors validCells visitedCells edgeStateMap dirs cell = do
  (maybeDir, dirs') <- getRandomDir dirs
  case maybeDir of
    Nothing -> return (edgeStateMap, visitedCells)
    Just dir -> do
      (edgeStateMap', visitedCells') <-
        visitNeighbor validCells visitedCells  edgeStateMap dir cell
      visitNeighbors validCells visitedCells' edgeStateMap' dirs' cell

visitCell ::
  ValidCells ->
  VisitedCells ->
  EdgeStateMap ->
  Cell ->
  IO (EdgeStateMap, VisitedCells)
visitCell validCells visitedCells edgeStateMap cell = do
  let visitedCells' =
        visitedCells { getVisitedCells = S.insert cell (getVisitedCells visitedCells) }
  visitNeighbors validCells visitedCells' edgeStateMap [minBound .. maxBound] cell

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
      validCells = ValidCells { getValidCells = S.fromList cells}
      visitedCells = VisitedCells { getVisitedCells = S.empty }
      initialEdgeStateMap = getInitialEdgeStateMap cells validCells
  (edgeStateMap, _) <- visitCell validCells visitedCells initialEdgeStateMap (head cells)
  return (rows, cols, edgeStateMap)

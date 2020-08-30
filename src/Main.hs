{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Text                   (Text, pack)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
-- Note: I was unable to import "main" and "main_" from Shpadoinkle.Html
-- but I could import "main'" and "main'_"
import           Shpadoinkle.Html            (div_, getBody, h1_, table, td, tr)
import           System.Random               (randomRIO)

import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S

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
getDirFns TopDir = (topNeighbor, topEdge)
getDirFns RightDir = (rightNeighbor, rightEdge)
getDirFns BottomDir = (bottomNeighbor, bottomEdge)
getDirFns LeftDir = (leftNeighbor, leftEdge)

isUnvisited :: S.Set Cell -> Cell -> Bool
isUnvisited visited neighbor = not . S.member neighbor $ visited

getRandomDir :: [Direction] -> IO (Maybe Direction, [Direction])
getRandomDir [] = return (Nothing, [])
getRandomDir dirs = do
  i <- randomRIO (0, length dirs - 1)
  let dir = dirs !! i
      dirs' = filter (/= dir) dirs
  return $ (Just dir, dirs')

generateMaze :: IO EdgeStateMap
generateMaze = do
  (edgeStateMap, _) <- go initialEdgeStateMap S.empty (head cells)
  return edgeStateMap
  where go :: EdgeStateMap -> S.Set Cell -> Cell -> IO (EdgeStateMap, S.Set Cell)
        go edgeStateMap visited cell = do
          let visited' = S.insert cell visited
          visitNeighbors edgeStateMap visited' cell [minBound .. maxBound]
        visitNeighbors :: EdgeStateMap -> S.Set Cell -> Cell -> [Direction] -> IO (EdgeStateMap, S.Set Cell)
        visitNeighbors edgeStateMap visited cell dirs = do
          (maybeDir, dirs') <- getRandomDir dirs
          case maybeDir of
            Nothing -> return (edgeStateMap, visited)
            Just dir -> do
              (edgeStateMap', visited') <- visitNeighbor edgeStateMap visited cell dir
              visitNeighbors edgeStateMap' visited' cell dirs'
        visitNeighbor :: EdgeStateMap -> S.Set Cell -> Cell -> Direction -> IO (EdgeStateMap, S.Set Cell)
        visitNeighbor edgeStateMap visited cell dir = do
          let (neighborFn, edgeFn) = getDirFns dir
              neighbor = neighborFn cell
          if isCellValid neighbor && isUnvisited visited neighbor
            then do
              let edgeStateMap' = M.insert (edgeFn cell) Open edgeStateMap
              go edgeStateMap' visited neighbor
            else return (edgeStateMap, visited)

data AppState = AppState { greeting :: String, edgeStateMap :: EdgeStateMap } deriving (Eq, Show)

-- View
style :: Text -> (Text, Prop m)
style t = ("style", PText t)

borderColors :: EdgeStateMap -> Cell -> Text
borderColors edgeStateMap cell = foldr1 (<>) $
  [ borderProp "top" . edgeState edgeStateMap . topEdge
  , borderProp "right" . edgeState edgeStateMap . rightEdge
  , borderProp "bottom" . edgeState edgeStateMap . bottomEdge
  , borderProp "left" . edgeState edgeStateMap . leftEdge
  ] <*> pure cell
  where
    color :: EdgeState -> Text
    color edgeState =
          case edgeState of
            Open -> "white"
            Wall -> "black"
    borderProp :: Text -> EdgeState -> Text
    borderProp dir edgeState =
      "border-" <> dir <> "-color: " <> (color edgeState) <> ";"

tCell :: EdgeStateMap -> Int -> Int -> Html AppState
tCell edgeStateMap r c = td [ css ] [ content ]
  where content = text . pack $ "(" <> show r <> ", " <> show c <> ")"
        css = style $
          "width: 3em;" <>
          "height: 3em;" <>
          "display: inline-flex;" <>
          "justify-content: center;" <>
          "align-items: center;" <>
          "border-width: 2px;" <>
          "border-style: solid;" <>
          borderColors edgeStateMap (r, c)

view :: AppState -> Html AppState
view appState = div_
  [ h1_ [ "The Overlook Maze" ]
  , div_ [ text . pack $ greeting appState ]
  , table [ css ] $
    map (\r -> tr [] $ map (\c -> tCell (edgeStateMap appState) r c) cols) rows
  ]
  where css = style $
          "border-spacing: 0;" <>
          "border: 2px solid black;"

main :: IO ()
main = do
  edgeStateMap' <- generateMaze
  let initialAppState = AppState { greeting = "Hello, Newman", edgeStateMap = edgeStateMap' } 
  runJSorWarp 8080 $
    simple runParDiff initialAppState (constly' . view) getBody

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
rows = [0..10]

cols :: [Int]
cols = [0..6]

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

edgeState :: Edge -> EdgeState
edgeState edge = fromMaybe Wall . M.lookup edge $ initialEdgeStateMap

getNeighbors :: Cell -> [Cell]
getNeighbors cell = filter isCellValid $
  [ topNeighbor
  , rightNeighbor
  , bottomNeighbor
  , leftNeighbor
  ] <*> pure cell

{-

[ n1, n2, n3 ]

n2



-}

generateMaze :: IO EdgeStateMap
generateMaze = go initialEdgeStateMap S.empty (head cells)
  where go :: EdgeStateMap -> S.Set Cell -> Cell -> IO EdgeStateMap
        go edgeStateMap visited cell = do
          let visited' = S.insert cell visited
              neighbors = getNeighbors cell
              unvisited = filter (isUnvisited visited') neighbors
          neighbor <- getRandomNeighbor unvisited
          return edgeStateMap
        isUnvisited :: S.Set Cell -> Cell -> Bool
        isUnvisited visited neighbor = not . S.member neighbor $ visited
        getRandomNeighbor unvisited = do
          i <- randomRIO (0, length unvisited - 1)
          return $ unvisited !! i



data AppState = AppState { greeting :: String } deriving (Eq, Show)

initialAppState :: AppState
initialAppState = AppState { greeting = "Hello, Newman" }

-- View
style :: Text -> (Text, Prop m)
style t = ("style", PText t)

borderColors :: Cell -> Text
borderColors cell = foldr1 (<>) $
  [ borderProp "top" . edgeState . topEdge
  , borderProp "right" . edgeState . rightEdge
  , borderProp "bottom" . edgeState . bottomEdge
  , borderProp "left" . edgeState . leftEdge
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

tCell :: Int -> Int -> Html AppState
tCell r c = td [ css ] [ content ]
  where content = text . pack $ "(" <> show r <> ", " <> show c <> ")"
        css = style $
          "width: 3em;" <>
          "height: 3em;" <>
          "display: inline-flex;" <>
          "justify-content: center;" <>
          "align-items: center;" <>
          "border-width: 2px;" <>
          "border-style: solid;" <>
          borderColors (r, c)

view :: AppState -> Html AppState
view appState = div_
  [ h1_ [ "The Overlook Maze" ]
  , div_ [ text . pack $ greeting appState ]
  , table [ css ] $
    map (\r -> tr [] $ map (\c -> tCell r c) cols) rows
  ]
  where css = style $
          "border-spacing: 0;" <>
          "border: 2px solid black;"

main :: IO ()
main = do
  runJSorWarp 8080 $
    simple runParDiff initialAppState (constly' . view) getBody

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Text                   (Text, pack)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
-- Note: I was unable to import "main" and "main_" from Shpadoinkle.Html
-- but I could import "main'" and "main'_"
import           Shpadoinkle.Html            (div_, getBody, h1_, table, td, tr)

import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S

type Cell = (Int, Int)

              -- Top  Bottom
              -- Left Right
data Edge = Edge Cell Cell deriving (Eq, Ord, Show)

data EdgeState = Open | Wall deriving (Eq, Show)

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
cols = [0..5]

cells :: [Cell]
cells = do
  r <- rows
  c <- cols
  return (r, c)

cellSet :: S.Set Cell
cellSet = S.fromList cells

maybeEdge :: (Cell -> Cell) -> (Cell -> Edge) -> Cell -> Maybe Edge
maybeEdge neighborF edgeF cell =
  if S.member (neighborF cell) cellSet
     then Just (edgeF cell)
     else Nothing     

edgeStateMap :: M.Map Edge EdgeState
edgeStateMap = M.fromList . map (flip (,) $ Wall) $ edges
  where edges = catMaybes $
          [ maybeEdge topNeighbor topEdge
          , maybeEdge rightNeighbor rightEdge
          , maybeEdge bottomNeighbor bottomEdge
          , maybeEdge leftNeighbor leftEdge
          ] <*> cells

edgeState :: Edge -> EdgeState
edgeState edge = fromMaybe Wall . M.lookup edge $ edgeStateMap

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

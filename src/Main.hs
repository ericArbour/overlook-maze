{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Data.Text                   (Text, pack)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
-- Note: I was unable to import "main" and "main_" from Shpadoinkle.Html
-- but I could import "main'" and "main'_"
import           Shpadoinkle.Html            (div_, getBody, h1_, table, td, tr)

import qualified Data.Map                    as M
import qualified Data.Set                    as S

type Cell = (Int, Int)

type Cells = S.Set Cell

data EdgeState = Open | Wall deriving (Eq, Show)

data Edge
  = HEdge Cell Cell
  | VEdge Cell Cell
  deriving (Eq, Show)

type EdgeMap = M.Map Edge EdgeState

top :: Cell -> Cell
top (r, c) = (r - 1, c)

right :: Cell -> Cell
right (r, c) = (r, c + 1)

bottom :: Cell -> Cell
bottom (r, c) = (r + 1, c)

left :: Cell -> Cell
left (r, c) = (r, c - 1)

getNeighbor :: Cell -> Cells -> (Cell -> Cell) -> Maybe Cell
getNeighbor cell cells f = if hasNeighbor then Just neighbor else Nothing
  where neighbor = f cell
        hasNeighbor = S.member neighbor cells

rows :: [Int]
rows = [0..10]

cols :: [Int]
cols = [0..5]

cells :: Cells
cells = S.fromList $ do
  r <- rows
  c <- cols
  return (r, c)

data AppState = AppState { greeting :: String } deriving (Eq, Show)

initialAppState :: AppState
initialAppState = AppState { greeting = "Hello, Newman" }

-- View
style :: Text -> (Text, Prop m)
style t = ("style", PText t)

tCell :: Int -> Int -> Html AppState
tCell r c = td [ css ] [ content ]
  where content = text . pack $ "(" <> show r <> ", " <> show c <> ")"
        css = style $
          "border: 2px solid black;" <>
          "width: 3em;" <>
          "height: 3em;" <>
          "display: inline-flex;" <>
          "justify-content: center;" <>
          "align-items: center;"

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
  putStrLn "test"
  runJSorWarp 8080 $
    simple runParDiff initialAppState (constly' . view) getBody

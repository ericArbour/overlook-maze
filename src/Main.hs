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

import           Maze

data AppState = AppState
  { greeting     :: String
  , edgeStateMap :: EdgeStateMap
  } deriving (Eq, Show)

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
  let initialAppState =
        AppState { greeting = "Hello, Newman", edgeStateMap = edgeStateMap' }
  runJSorWarp 8080 $
    simple runParDiff initialAppState (constly' . view) getBody

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Text                   (Text, pack, unpack)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
-- Note: I was unable to import "main" and "main_" from Shpadoinkle.Html
-- but I could import "main'" and "main'_"
import           Shpadoinkle.Html            (div_, getBody, h1_, option,
                                              select, table, td, tr)
import           Shpadoinkle.Html.Event      (onOptionM)
import           Shpadoinkle.Html.Property   (selected, value)
import           System.Random               (randomRIO)

import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S

import           Maze

data AppState = AppState
  { mazeSize :: MazeSize
  , maze     :: Maze
  } deriving (Eq, Show)

style :: Text -> (Text, PropM m o)
style t = ("style", textProp t)

borderColors :: EdgeStateMap -> Cell -> Text
borderColors edgeStateMap cell = foldr1 (<>) $
  [ borderProp "top" . getEdgeState edgeStateMap . getEdge TopDir
  , borderProp "right" . getEdgeState edgeStateMap . getEdge RightDir
  , borderProp "bottom" . getEdgeState edgeStateMap . getEdge BottomDir
  , borderProp "left" . getEdgeState edgeStateMap . getEdge LeftDir
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

tCell :: MonadIO m => EdgeStateMap -> Int -> Int -> HtmlM m AppState
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

sizeOption :: MonadIO m => MazeSize -> MazeSize -> HtmlM m AppState
sizeOption currentSize size = option
      [ value . pack $ show size, selected $ currentSize == size ]
      [ text . pack $ show size ]

sizeSelect :: MonadIO m => AppState -> HtmlM m AppState
sizeSelect appState = select [ onOptionM handleOption ]
  $ sizeOption (mazeSize appState) <$> [ minBound .. maxBound ]
  where
    handleOption msText = do
      let mazeSize' = read $ unpack msText
      maze' <- liftIO $ generateMaze mazeSize'
      return $ updateState maze' mazeSize'
    updateState maze' mazeSize' appState' =
      appState' { maze = maze'
                , mazeSize = mazeSize'
                }

view :: MonadIO m => AppState -> HtmlM m AppState
view appState = div_
  [ h1_ [ "The Overlook Maze" ]
  , sizeSelect appState
  , table [ css ] $
    map (\r -> tr [] $ map (\c -> tCell edgeStateMap r c) cols) rows
  ]
  where (rows, cols, edgeStateMap) = maze appState
        css = style $
          "border-spacing: 0;" <>
          "border: 2px solid black;"

main :: IO ()
main = do
  initialMaze <- generateMaze Medium
  let initialAppState =
        AppState { mazeSize = Medium, maze = initialMaze }
  runJSorWarp 8080 $
    simple runParDiff initialAppState view getBody

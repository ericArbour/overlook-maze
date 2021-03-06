{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Text                   (Text, pack, unpack)
import           Prelude                     hiding (div)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
-- Note: I was unable to import "main" and "main_" from Shpadoinkle.Html
-- but I could import "main'" and "main'_"
import           Shpadoinkle.Html            (div, getBody, h1_, option, select,
                                              table, td, tr)
import           Shpadoinkle.Html.Event      (onKeydown, onOptionM)
import           Shpadoinkle.Html.Property   (selected, tabbable, value)
import           Shpadoinkle.Keyboard
import           System.Random               (randomRIO)

import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S

import           Maze

data AppState = AppState
  { mazeSize   :: MazeSize
  , maze       :: Maze
  , playerCell :: Cell
  } deriving (Eq, Show)

style :: Text -> (Text, PropM m o)
style t = ("style", textProp t)

borderColors :: Maze -> Cell -> Text
borderColors maze cell = foldr1 (<>) $
  [ borderProp "top" . hasWall maze TopDir
  , borderProp "right" . hasWall maze RightDir
  , borderProp "bottom" . hasWall maze BottomDir
  , borderProp "left" . hasWall maze LeftDir
  ] <*> pure cell
  where
    color :: Bool -> Text
    color isWall =
      if isWall
      then "var(--wall-color);"
      else "var(--open-color);"
    borderProp :: Text -> Bool -> Text
    borderProp dir isWall =
      "border-" <> dir <> "-color: " <> color isWall <> ";"

tCell :: MonadIO m => AppState -> Cell -> HtmlM m AppState
tCell appState cell = td [ css ] [ content ]
  where content = text . pack $ if playerCell appState == cell then "X" else ""
        css = style $
          "width: calc(1em + 2vmin);" <>
          "height: calc(1em + 2vmin);" <>
          "display: inline-flex;" <>
          "justify-content: center;" <>
          "align-items: center;" <>
          "border-width: 2px;" <>
          "border-style: solid;" <>
          borderColors (maze appState) cell

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
                , playerCell = (0, 0)
                }

playerStep :: AppState -> Direction -> AppState
playerStep appState dir =
  case mazeStep (maze appState) dir (playerCell appState) of
    Just neighbor -> appState { playerCell = neighbor }
    Nothing       -> appState

handleKeydown :: AppState -> KeyCode -> AppState
handleKeydown appState UpArrow    = playerStep appState TopDir
handleKeydown appState RightArrow = playerStep appState RightDir
handleKeydown appState DownArrow  = playerStep appState BottomDir
handleKeydown appState LeftArrow  = playerStep appState LeftDir
handleKeydown appState _          = appState

view :: MonadIO m => AppState -> HtmlM m AppState
view appState = div [ containerCss ]
  [ h1_ [ "The Overlook Maze" ]
  , sizeSelect appState
  , table [ tabbable, onKeydown (handleKeydown appState), tableCss ] $
      mapRows (maze appState) $ \r -> tr [] $
        mapCols (maze appState) $ \c -> tCell appState (r, c)
  ]
  where
    containerCss = style $
      "--wall-color: #27394d;" <>
      "--open-color: #6381b7;" <>
      "background-color: var(--wall-color);" <>
      "color: white;" <>
      "padding: 1em;"
    tableCss = style $
      "margin: 0 auto;" <>
      "border-spacing: 0;" <>
      "border: 2px solid var(--wall-color);" <>
      "background-color: var(--open-color);"


main :: IO ()
main = do
  initialMaze <- generateMaze Medium
  let initialAppState =
        AppState { mazeSize = Medium, maze = initialMaze, playerCell = (0, 0) }
  runJSorWarp 8080 $
    simple runParDiff initialAppState view getBody

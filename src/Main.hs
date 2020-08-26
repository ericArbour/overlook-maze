{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Data.Text                   (Text, pack)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
-- Note: I was unable to import "main" and "main_" from Shpadoinkle.Html
-- but I could import "main'" and "main'_"
import           Shpadoinkle.Html            (div_, getBody, h1_, li_, ul)

import qualified Data.Map                    as M

data Cell = Cell
  { top    :: Maybe Cell
  , right  :: Maybe Cell
  , bottom :: Maybe Cell
  , left   :: Maybe Cell
  } deriving (Eq, Show)

type Coord = (Int, Int)

type CellMap = M.Map Coord Cell

data DividerState = Open | Wall deriving (Eq, Show)

data Divider
  = HDivider (Maybe Cell) (Maybe Cell)
  | VDivider (Maybe Cell) (Maybe Cell)
  deriving (Eq, Show)

data AppState = AppState { greeting :: String } deriving (Eq, Show)

initialAppState :: AppState
initialAppState = AppState { greeting = "Hello, Newman" }

{-

(0,0) : Cell { empty, topD = (VDivider Wall Nothing (Just Cell)), rightD = (HDivider Wall

-}

view :: AppState -> Html AppState
view appState = div_
  [ h1_ [ "The Overlook Maze" ]
  , div_ [ text . pack $ greeting appState ]
  , ul "test-class"
    [ li_ [ "test" ]
    , li_ [ "test" ]
    , li_ [ "test" ]
    ]
  ]


main :: IO ()
main = do
  putStrLn "test"
  runJSorWarp 8080 $
    simple runParDiff initialAppState (constly' . view) getBody

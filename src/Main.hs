{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.Text (pack, Text)

import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html (getBody, div_, h1_)

data AppState = AppState { greeting :: String } deriving (Eq, Show)

initialAppState :: AppState
initialAppState = AppState { greeting = "Hello, Newman" }

view :: AppState -> Html AppState
view appState = div_
  [ h1_ [ "The Overlook Maze" ]
  , div_ [ text . pack $ greeting appState ]
  ]


main :: IO ()
main = runJSorWarp 8080 $
  simple runParDiff initialAppState (constly' . view) getBody

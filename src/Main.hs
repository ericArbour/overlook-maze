{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html


view :: () -> Html m ()
view _ = "hello world"


main :: IO ()
main = runJSorWarp 8080 $
  simple runParDiff () view getBody

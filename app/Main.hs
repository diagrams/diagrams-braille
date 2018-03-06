{-# LANGUAGE FlexibleContexts #-}
module Main where

import Diagrams.Backend.Braille.CmdLine
import Diagrams.Prelude

d :: Diagram B
d = f 1 `atop` f (sqrt 2 / 2) `atop` f (sqrt 2 * (sqrt 2 / 2) / 2) where
  f x = circle x `atop` square (x * sqrt 2)

main :: IO ()
main = multiMain [
    ("triangle", triangle 1)
  , ("square", square 1)
  , ("pentagon", pentagon 1)
  , ("hexagon", hexagon 1)
  , ("heptagon", heptagon 1)
  , ("octagon", octagon 1)
  , ("nonagon", nonagon 1)
  , ("decagon", decagon 1)
  , ("hendecagon", hendecagon 1)
  , ("dodecagon", dodecagon 1)
  , ("ellipse", ellipse 0.5)
  , ("circle", circle 1)
  ]

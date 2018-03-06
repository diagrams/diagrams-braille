{-# LANGUAGE FlexibleContexts #-}
module Main where

import Diagrams.Backend.Braille.CmdLine
import Diagrams.Prelude

d = f 1 `atop` f (sqrt 2 / 2) `atop` f (sqrt 2 * (sqrt 2 / 2) / 2) where
  f x = circle x `atop` square (x * sqrt 2)

main :: IO ()
main = defaultMain d

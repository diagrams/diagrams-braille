{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Diagrams.Prelude
import Diagrams.Coordinates
import Diagrams.Backend.Braille.CmdLine
import Data.Time

clock :: UTCTime -> Diagram B
clock t = circle 0.35 # fc silver # lwG 0
       <> bigHand # f 12 h <> littleHand # f 60 m
       <> circle 1  # fc black # lwG 0
       <> circle 11 # lwG 1.5 # lc slategray -- # fc lightsteelblue
  where
    s = realToFrac $ utctDayTime t :: Double
    m = s / 60
    h = m / 60
    bigHand    = (0 ^& (-1.5)) ~~ (0 ^& 7.5) # lwG 0.5
    littleHand = (0 ^& (-2))   ~~ (0 ^& 9.5) # lwG 0.2
    f n v = rotate (- v / n @@ turn)

main = mainWith $ clock <$> getCurrentTime

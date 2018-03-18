{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Diagrams.Backend.Braille.CmdLine
import Diagrams.Backend.CmdLine (Parseable(..))
import Diagrams.Prelude hiding (option)
import Options.Applicative

d = f 1 `atop` f (sqrt 2 / 2) `atop` f (sqrt 2 * (sqrt 2 / 2) / 2) where
  f x = circle x `atop` square (x * sqrt 2)

sol = circle 1 <> atPoints (hexagon 1) (repeat $ circle 1)
fol = circle 1 <> atPoints (hexagon 1) (map (`rotateBy` pedal) [0, 1/6 ..])
pedal = circle 1 <> circle 1 # translateX 1 # rotateBy (-1/3)

clock h m = circle 0.35 # fc silver # lwG 0
         <> bigHand # f 12 h <> littleHand # f 60 m
         <> circle 1  # fc black # lwG 0
         <> circle 11 # lwG 1.5 # lc slategray -- # fc lightsteelblue
  where
    bigHand    = (0 ^& (-1.5)) ~~ (0 ^& 7.5) # lwG 0.5
    littleHand = (0 ^& (-2))   ~~ (0 ^& 9.5) # lwG 0.2
    f n v = rotate (- v / n @@ turn)

hilbert 0 = mempty
hilbert n = hilbert' (n-1) # reflectY <> vrule 1
         <> hilbert  (n-1) <> hrule 1
         <> hilbert  (n-1) <> vrule (-1)
         <> hilbert' (n-1) # reflectX
  where hilbert' = rotateBy (1/4) . hilbert

newtype Opts = Opts { draw :: Diagram B }

instance Parseable Opts where
  parser = fmap (Opts . mconcat) $ some $ hsubparser $ mconcat [
      cmd "circle" "A circle of the given radius, centered at the origin." $
      circle <$> arg (metavar "RADIUS")
    , cmd "clock" "A clock showing HOUR and MINUTE." $
      clock <$> arg (metavar "HOUR") <*> arg (metavar "MINUTE")
    , cmd "decagon" "" $ decagon <$> arg mempty
    , cmd "dodecagon" "" $ dodecagon <$> arg mempty
    , cmd "flower-of-life" "" (pure fol)
    , cmd "hendecagon" "" $ hendecagon <$> arg mempty
    , cmd "heptagon" "" $ heptagon <$> arg mempty
    , cmd "hexagon" "" $ hexagon <$> arg mempty
    , cmd "hilbert" "" $ strokeT . hilbert <$> arg (metavar "ORDER")
    , cmd "nonagon" "" $ nonagon <$> arg mempty
    , cmd "octagon" "" $ octagon <$> arg mempty
    , cmd "pentagon" "" $ pentagon <$> arg mempty
    , cmd "seed-of-life" "" (pure sol)
    , cmd "square" "" $ square <$> arg mempty
    , cmd "triangle" "" $ triangle <$> arg mempty
    ] where cmd s d p = command s $ info (foldr ($) <$> p <*> many modifier) $
                        progDesc d
            arg = argument auto

modifier = flag' centerXY (long "centerXY")
       <|> rotateBy <$> option auto (long "rotateBy")
       <|> flag' showOrigin (long "showOrigin")
       <|> translateX <$> option auto (long "translateX")
       <|> translateY <$> option auto (long "translateY")

main = mainWith draw

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Diagrams.Backend.Braille.CmdLine
import Diagrams.Backend.CmdLine (Parseable(..))
import Diagrams.Prelude hiding (option, value)
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
      commandGroup "Diagrams"
    , cmd "hrule" "A centered horizontal (L-R) line of the given length." $
      hrule <$> arg (metavar "LENGTH")
    , cmd "vrule" "A centered vertical (T-B) line of the given length." $
      vrule <$> arg (metavar "LENGTH")
    , cmd "triangle" "An equilateral triangle, with sides of the given length and base parallel to the x-axis." $
      triangle <$> arg (metavar "LENGTH")
    , cmd "rect" "An axis-aligned rectangle of the given width and height, centered at the origin." $
      rect <$> arg (metavar "WIDTH")
           <*> arg (metavar "HEIGHT")
    , cmd "roundedRect" "An axis-aligned rectangle with the given width and height with circular rounded corners of radius, centered at the origin." $
      roundedRect <$> arg (metavar "WIDTH")
                  <*> arg (metavar "HEIGHT")
                  <*> arg (metavar "RADIUS")
    , cmd "square" "A square with its center at the origin and sides of the given length, oriented parallel to the axes." $
      square <$> arg (metavar "LENGTH")
    , cmd "pentagon" "A regular pentagon, with sides of the given length and base parallel to the x-axis." $
      pentagon <$> arg (metavar "LENGTH")
    , cmd "hexagon" "A regular hexagon, with sides of the given length and base parallel to the x-axis." $
      hexagon <$> arg (metavar "LENGTH")
    , cmd "heptagon" "A regular heptagon, with sides of the given length and base parallel to the x-axis." $
      heptagon <$> arg (metavar "LENGTH")
    , cmd "octagon" "A regular octagon, with sides of the given length and base parallel to the x-axis." $
      octagon <$> arg (metavar "LENGTH")
    , cmd "nonagon" "A regular nonagon, with sides of the given length and base parallel to the x-axis." $
      nonagon <$> arg (metavar "LENGTH")
    , cmd "decagon" "A regular decagon, with sides of the given length and base parallel to the x-axis." $
      decagon <$> arg (metavar "LENGTH")
    , cmd "hendecagon" "A regular hendecagon, with sides of the given length and base parallel to the x-axis." $
       hendecagon <$> arg (metavar "LENGTH")
    , cmd "dodecagon" "A regular dodecagon, with sides of the given length and base parallel to the x-axis." $
      dodecagon <$> arg (metavar "LENGTH")
    , cmd "circle" "A circle of the given radius, centered at the origin." $
      circle <$> arg (metavar "RADIUS")
    , cmd "text" "Print text." $ text <$> strArgument (metavar "STRING")
    , cmd "baselineText" "Print text." $ baselineText <$> strArgument (metavar "STRING")
    , cmd "clock" "A clock showing the time given in hours and minutes." $
      clock <$> arg (metavar "HOUR")
            <*> arg (metavar "MINUTE")
    , cmd "flower-of-life" "The \"flower of life\"." $
      pure fol
    , cmd "hilbert" "A hilbert curve of the given order." $
      strokeT . hilbert <$> arg (metavar "ORDER")
    , cmd "seed-of-life" "The \"seed of life\"." $ pure sol
    ] where cmd s d p = command s $ info (foldr ($) <$> p <*> many modifier) $
                        progDesc d
            arg = argument auto

modifier = flag' alignL (long "alignL") <|> flag' alignR (long "alignR")
       <|> flag' alignT (long "alignT") <|> flag' alignB (long "alignB")
       <|> flag' centerX (long "centerX" <> help "Center the local origin along the X-axis.")
       <|> flag' centerY (long "centerY" <> help "Center the local origin along the Y-axis.")
       <|> flag' centerXY (long "centerXY" <> help "Center along both the X- and Y-axes.")
       <|> (named :: String -> Diagram B -> Diagram B) <$> option auto (long "named" <> metavar "NAME")
       <|> flag' reflectX (long "reflectX" <> help "Flip a diagram from left to right, i.e. send the point (x,y) to (-x,y).")
       <|> flag' reflectY (long "reflectY" <> help "Flip a diagram from top to bottom, i.e. send the point (x,y) to (x,-y).")
       <|> flag' reflectXY (long "reflectXY" <> help "Flips the diagram about x=y, i.e. send the point (x,y) to (y,x).")
       <|> rotateBy <$> option auto (long "rotateBy")
       <|> scaleX <$> option auto (long "scaleX" <> metavar "FACTOR" <> help "Scale a diagram by the given factor in the x (horizontal) direction.")
       <|> scaleY <$> option auto (long "scaleY" <> metavar "FACTOR" <> help "Scale a diagram by the given factor in the y (vertical) direction.")
       <|> flag' showOrigin (long "showOrigin")
       <|> translateX <$> option auto (long "translateX")
       <|> translateY <$> option auto (long "translateY")

main = mainWith draw

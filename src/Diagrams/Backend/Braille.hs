{-# LANGUAGE FlexibleContexts #-}
module Diagrams.Backend.Braille (renderBraille, rasterBraille, img2brl) where

import Codec.Picture (PixelRGBA8(PixelRGBA8), imageWidth, imageHeight, pixelAt)
import Data.Bits (setBit)
import Data.Char (chr)
import Diagrams.Core (renderDia)
import Diagrams.Backend.Rasterific

rasterBraille sz = img2brl . renderDia Rasterific (RasterificOptions sz)

renderBraille fp sz = writeFile fp . rasterBraille sz

img2brl = img2brl' 8 f where
  f (PixelRGBA8 _ _ _ a) | a > 20 = True
  f _ = False

img2brl' dots set img = unlines $
                        map (\y -> map (f y) columnIndices) lineIndices where
  f y x = chr $ foldr ($) 0x2800 $ take dots $ zipWith ($) [
                                    g y x       True
    , let y' = y+1 in               g y' x    $ y' < h
    , let y'' = y+2 in              g y'' x   $ y'' < h
    , let x' = x+1 in               g y x'    $ x' < w
    , let {y' = y+1; x' = x+1} in   g y' x'   $ y' < h && x' < w
    , let {y'' = y+2; x' = x+1} in  g y'' x'  $ y'' < h && x' < w
    , let y''' = y+3 in             g y''' x  $ y''' < h
    , let {y''' = y+3; x' = x+1} in g y''' x' $ y''' < h && x' < w] [0..]
  g y x True b a | set $ pixelAt img x y = setBit a b
  g _ _ _ _ a = a
  lineIndices = [0, (dots `div` 2) .. h - 1]
  columnIndices = [0, 2 .. w - 1]
  (h, w) = (imageHeight img, imageWidth img)

{-# LANGUAGE FlexibleContexts #-}
module Diagrams.Backend.Braille (img2brl, rasterBraille) where

import Codec.Picture (PixelRGBA8(PixelRGBA8), imageWidth, imageHeight, pixelAt)
import Data.Char (chr)
import Diagrams.Core (renderDia)
import Diagrams.Backend.Rasterific

rasterBraille sz = img2brl . renderDia Rasterific (RasterificOptions sz)

img2brl img = unlines $ map (\y -> map (f y) columnIndices) lineIndices where
  f y x = chr $ foldr (g (y,x)) 0x2800 $ zip offsets dotValues
  g (y,x) ((y',x'), v) acc
    | y+y' < imageHeight img && x+x' < imageWidth img =
      case pixelAt img (x+x') (y+y') of
        PixelRGBA8 _ _ _ a -> if a > 20 then acc + v else acc
    | otherwise = acc
  lineIndices = [0, 4 .. imageHeight img - 1]
  columnIndices = [0, 2 .. imageWidth img - 1]
  offsets = [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(3,0),(3,1)]
  dotValues = [1,2,4,8,16,32,64,128]

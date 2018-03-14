{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-------------------------------------------------------------------------------
-- | A rendering backend for Braille diagrams using Rasterific,
-- implemented natively in Haskell (making it easy to use on any
-- platform).
--
-- To invoke the Braille backend, you have three options.
--
-- * You can use the "Diagrams.Backend.Braille.CmdLine" module to create
--   standalone executables which output images when invoked.
--
-- * You can use the 'renderBraille' function provided by this module,
--   which gives you more flexible programmatic control over when and
--   how images are output (making it easy to, for example, write a
--   single program that outputs multiple images, or one that outputs
--   images dynamically based on user input, and so on).
--
-- * For the most flexibility (/e.g./ if you want access to the
--   resulting Braille value directly in memory without writing it to
--   disk), you can manually invoke the 'renderDia' method from the
--   'Diagrams.Core.Types.Backend' instance for @Braille@.  In particular,
--   'Diagrams.Core.Types.renderDia' has the generic type
--
-- > renderDia :: b -> Options b v n -> QDiagram b v n m -> Result b v n
--
-- (omitting a few type class constraints).  @b@ represents the
-- backend type, @v@ the vector space, @n@ the numeric field, and @m@ the type
-- of monoidal query annotations on the diagram.  'Options' and 'Result' are
-- associated data and type families, respectively, which yield the
-- type of option records and rendering results specific to any
-- particular backend.  For @b ~ Braille@, @v ~ V2@, and @n ~ n@, we have
--
-- > data Options Braille V2 n = BrailleOptions
-- >        { _size      :: SizeSpec2D n -- ^ The requested size of the output
-- >        }
--
-- @
-- type family Result Braille V2 n = String
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: Braille -> Options Braille V2 n -> QDiagram Braille V2 n m -> String
-- @
--
-- which you could call like @renderDia Braille (BrailleOptions (mkWidth 80))
-- myDiagram@.
--
-------------------------------------------------------------------------------
module Diagrams.Backend.Braille
  ( -- * Braille backend
    Braille(..)
  , B -- rendering token
  , Options(..)

    -- * Rendering
  , rasterBraille
  , renderBraille
  , size
  ) where

import           Codec.Picture
import           Codec.Picture.Types                 (convertImage,
                                                      promoteImage)
import           Control.Lens                        hiding ((#), transform)
import           Control.Monad                       (when)
import           Control.Monad.Reader                (ReaderT, runReaderT, ask, local)
import           Control.Monad.Writer                (Writer, execWriter, tell)
import           Data.Bits                           (setBit)
import           Data.Char                           (chr)
import           Data.Foldable                       (foldMap)
import           Data.Hashable                       (Hashable(..))
import           Data.Maybe                          (fromMaybe)
import           Data.Tree
import           Data.Typeable
import           Diagrams.Backend.Rasterific.Text
import           Diagrams.Core.Compile
import           Diagrams.Core.Transform             (matrixHomRep)
import           Diagrams.Core.Types
import           Diagrams.Prelude                    hiding (local)
import           Diagrams.TwoD.Adjust                (adjustDia2D)
import           Diagrams.TwoD.Text                  hiding (Font)
import qualified Graphics.Rasterific                 as R
import           Graphics.Rasterific.Texture         (Gradient,
                                                      linearGradientTexture,
                                                      radialGradientWithFocusTexture,
                                                      transformTexture,
                                                      uniformTexture,
                                                      withSampler)
import qualified Graphics.Rasterific.Transformations as R
import           System.FilePath                     (takeExtension)

data Braille = Braille deriving (Eq, Ord, Read, Show, Typeable)

type B = Braille

type instance V Braille = V2
type instance N Braille = Double

-- | The custom monad in which intermediate drawing options take
--   place; 'Graphics.Rasterific.Drawing' is Rasterific's own rendering
--   monad.
type RenderM n = ReaderT (Style V2 n) (Writer Draw)

newtype Draw = Draw (R.Drawing PixelRGBA8 (), [((Int, Int), String)])

instance Monoid Draw where
  mempty = Draw (return (), mempty)
  Draw (m1, l1) `mappend` Draw (m2, l2) = Draw (m1 >> m2, l1 <> l2)

type RenderR = R.Drawing PixelRGBA8

tellR :: RenderR () -> RenderM n ()
tellR r = tell $ Draw (r, mempty)

tellT :: Int -> Int -> String -> RenderM n ()
tellT x y t = tell $ Draw (pure (), [((x, y), t)])

runRenderM :: TypeableFloat n => RenderM n a -> Draw
runRenderM = execWriter . (`runReaderT` sty) where
  sty = mempty # recommendFillColor transparent

-- From Diagrams.Core.Types.
instance TypeableFloat n => Backend Braille V2 n where
  newtype Render  Braille V2 n = R (RenderM n ())
  type Result  Braille V2 n = String
  data Options Braille V2 n = BrailleOptions
          { _sizeSpec  :: SizeSpec V2 n -- ^ The requested size of the output
          } deriving Show

  renderRTree _ opts t =
    foldr drawText (img2brl (R.renderDrawing (round w) (round h) bgColor r)) txt
    where
      Draw (r, txt) = runRenderM . runR . fromRTree $ t
      V2 w h  = specToSize 100 (opts^.sizeSpec)
      bgColor = PixelRGBA8 0 0 0 0

  adjustDia c opts d = adjustDia2D sizeSpec c opts (d # reflectY)

drawText ((x, y), t) = unlines . flip (foldr $ uncurry f) (zip [x..] t) . lines where
  f x' = set $ element y . element x'

fromRTree :: TypeableFloat n => RTree Braille V2 n Annotation -> Render Braille V2 n
fromRTree (Node n rs) = case n of
  RPrim p                 -> render Braille p
  RStyle sty              -> R $ local (<> sty) r
  _                       -> R r
  where R r = foldMap fromRTree rs

runR :: Render Braille V2 n -> RenderM n ()
runR (R r) = r

instance Monoid (Render Braille V2 n) where
  mempty = R $ return ()
  R rd1 `mappend` R rd2 = R (rd1 >> rd2)

instance Hashable n => Hashable (Options Braille V2 n) where
  hashWithSalt s (BrailleOptions sz) = s `hashWithSalt` sz

sizeSpec :: Lens' (Options Braille V2 n) (SizeSpec V2 n)
sizeSpec = lens _sizeSpec (\o s -> o {_sizeSpec = s})

rasterificStrokeStyle :: TypeableFloat n => Style v n
                     -> (n, R.Join, (R.Cap, R.Cap), Maybe (R.DashPattern, n))
rasterificStrokeStyle s = (strokeWidth, strokeJoin, (strokeCap, strokeCap), strokeDash)
  where
    strokeWidth = views _lineWidthU (fromMaybe 1) s
    strokeJoin  = views _lineJoin   fromLineJoin s
    strokeCap   = views _lineCap    fromLineCap s
    strokeDash  = views _dashingU   (fmap fromDashing) s

fromLineCap :: LineCap -> R.Cap
fromLineCap LineCapButt   = R.CapStraight 0
fromLineCap LineCapRound  = R.CapRound
fromLineCap LineCapSquare = R.CapStraight 1

fromLineJoin :: LineJoin -> R.Join
fromLineJoin LineJoinMiter = R.JoinMiter 0
fromLineJoin LineJoinRound = R.JoinRound
fromLineJoin LineJoinBevel = R.JoinMiter 1

fromDashing :: Real n => Dashing n -> (R.DashPattern, n)
fromDashing (Dashing ds d) = (map realToFrac ds, d)

fromFillRule :: FillRule -> R.FillMethod
fromFillRule EvenOdd = R.FillEvenOdd
fromFillRule _       = R.FillWinding

rasterificColor :: SomeColor -> Double -> PixelRGBA8
rasterificColor c o = PixelRGBA8 r g b a
  where
    (r, g, b, a) = (int r', int g', int b', int (o * a'))
    (r', g', b', a') = colorToSRGBA (toAlphaColour c)
    int x = round (255 * x)

rasterificSpreadMethod :: SpreadMethod -> R.SamplerRepeat
rasterificSpreadMethod GradPad     = R.SamplerPad
rasterificSpreadMethod GradReflect = R.SamplerReflect
rasterificSpreadMethod GradRepeat  = R.SamplerRepeat

rasterificStops :: TypeableFloat n => [GradientStop n] -> Gradient PixelRGBA8
rasterificStops = map fromStop
  where
    fromStop (GradientStop c v) = (realToFrac v, rasterificColor c 1)

rasterificLinearGradient :: TypeableFloat n => LGradient n -> R.Texture PixelRGBA8
rasterificLinearGradient g = transformTexture tr tx
  where
    tr = rasterificMatTransf (inv $ g^.lGradTrans)
    tx = withSampler spreadMethod (linearGradientTexture gradDef p0 p1)
    spreadMethod = rasterificSpreadMethod (g^.lGradSpreadMethod)
    gradDef = rasterificStops (g^.lGradStops)
    p0 = p2v2 (g^.lGradStart)
    p1 = p2v2 (g^.lGradEnd)

rasterificRadialGradient :: TypeableFloat n => RGradient n -> R.Texture PixelRGBA8
rasterificRadialGradient g = transformTexture tr tx
  where
    tr = rasterificMatTransf (inv $ g^.rGradTrans)
    tx = withSampler spreadMethod (radialGradientWithFocusTexture gradDef c (realToFrac r1) f)
    spreadMethod = rasterificSpreadMethod (g^.rGradSpreadMethod)
    c = p2v2 (g^.rGradCenter1)
    f = p2v2 (g^.rGradCenter0)
    gradDef = rasterificStops ss

    -- Adjust the stops so that the gradient begins at the perimeter of
    -- the inner circle (center0, radius0) and ends at the outer circle.
    r0 = g^.rGradRadius0
    r1 = g^.rGradRadius1
    stopFracs = r0 / r1 : map (\s -> (r0 + (s^.stopFraction) * (r1 - r0)) / r1)
                (g^.rGradStops)
    gradStops = case g^.rGradStops of
      []       -> []
      xs@(x:_) -> x : xs
    ss = zipWith (\gs sf -> gs & stopFraction .~ sf ) gradStops stopFracs

-- Convert a diagrams @Texture@ and opacity to a rasterific texture.
rasterificTexture :: TypeableFloat n => Texture n -> Double -> R.Texture PixelRGBA8
rasterificTexture (SC c) o = uniformTexture $ rasterificColor c o
rasterificTexture (LG g) _ = rasterificLinearGradient g
rasterificTexture (RG g) _ = rasterificRadialGradient g

p2v2 :: Real n => P2 n -> R.Point
p2v2 (P v) = r2v2 v
{-# INLINE p2v2 #-}

r2v2 :: Real n => V2 n -> R.Point
r2v2 (V2 x y) = R.V2 (realToFrac x) (realToFrac y)
{-# INLINE r2v2 #-}

rv2 :: (Real n, Fractional n) => Iso' R.Point (P2 n)
rv2 = iso (\(R.V2 x y) -> V2 (realToFrac x) (realToFrac y)) r2v2 . from _Point
{-# INLINE rv2 #-}

rasterificPtTransf :: TypeableFloat n => T2 n -> R.Point -> R.Point
rasterificPtTransf t = over rv2 (papply t)

rasterificMatTransf :: TypeableFloat n => T2 n -> R.Transformation
rasterificMatTransf tr = R.Transformation a c e b d f
  where
    [[a, b], [c, d], [e, f]] = map realToFrac <$> matrixHomRep tr

-- Note: Using view patterns confuses ghc to think there are missing patterns,
-- so we avoid them here.
renderSeg :: TypeableFloat n => Located (Segment Closed V2 n) -> R.Primitive
renderSeg l =
  case viewLoc l of
    (p, Linear (OffsetClosed v)) ->
      R.LinePrim $ R.Line p' (p' + r2v2 v)
      where
        p' = p2v2 p
    (p, Cubic u1 u2 (OffsetClosed u3)) ->
      R.CubicBezierPrim $ R.CubicBezier q0 q1 q2 q3
      where
        (q0, q1, q2, q3) = (p2v2 p, q0 + r2v2 u1, q0 + r2v2 u2, q0 + r2v2 u3)

renderPath :: TypeableFloat n => Path V2 n -> [[R.Primitive]]
renderPath p = (map . map) renderSeg (pathLocSegments p)

-- Stroke both dashed and solid lines.
mkStroke :: TypeableFloat n => n ->  R.Join -> (R.Cap, R.Cap) -> Maybe (R.DashPattern, n)
      -> [[R.Primitive]] -> RenderR ()
mkStroke (realToFrac -> l) j c d primList =
  maybe (mapM_ (R.stroke l j c) primList)
        (\(dsh, off) -> mapM_ (R.dashedStrokeWithOffset (realToFrac off) dsh l j c) primList)
        d

instance TypeableFloat n => Renderable (Path V2 n) Braille where
  render _ p = R $ do
    sty <- ask
    let f = sty ^. _fillTexture
        s = sty ^. _lineTexture
        o = sty ^. _opacity
        r = sty ^. _fillRule

        (l, j, c, d) = rasterificStrokeStyle sty
        canFill      = anyOf (_head . located) isLoop p && (f ^? _AC) /= Just transparent
        rule         = fromFillRule r

        -- For stroking we need to keep all of the contours separate.
        primList = renderPath p

        -- For filling we need to concatenate them into a flat list.
        prms = concat primList

    when canFill $
      tellR (R.withTexture (rasterificTexture f o) $ R.fillWithMethod rule prms)

    tellR (R.withTexture (rasterificTexture s o) $ mkStroke l j c d primList)

instance TypeableFloat n => Renderable (Text n) Braille where
  render _ (Text tr al str) = R $ do
    fs    <- views _fontSizeU (fromMaybe 12)
    slant <- view _fontSlant
    fw    <- view _fontWeight
    let fs'    = R.PointSize (realToFrac fs)
        fnt    = fromFontStyle slant fw
        bb     = textBoundingBox fnt fs' str
        P (V2 x y) = transform tr $ case al of
          BaselineText         -> 0 ^& 0
          BoxAlignedText xt yt -> case getCorners bb of
            Just (P (V2 xl yl), P (V2 xu yu)) -> (-lerp' xt xu xl) ^& lerp' yt yu yl
            Nothing                           -> 0 ^& 0
    tellT (round $ x / 2) (round $ y / 4) str
    where
      lerp' t u v = realToFrac $ t * u + (1 - t) * v

toImageRGBA8 :: DynamicImage -> Image PixelRGBA8
toImageRGBA8 (ImageRGBA8 i)  = i
toImageRGBA8 (ImageRGB8 i)   = promoteImage i
toImageRGBA8 (ImageYCbCr8 i) = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 (ImageY8 i)     = promoteImage i
toImageRGBA8 (ImageYA8 i)    = promoteImage i
toImageRGBA8 (ImageCMYK8 i)  = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 _               = error "Unsupported Pixel type"

instance TypeableFloat n => Renderable (DImage n Embedded) Braille where
  render _ (DImage iD w h tr) = R $ tellR
                               (R.withTransformation
                               (rasterificMatTransf (tr <> reflectionY))
                               (R.drawImage img 0 p))
    where
      ImageRaster dImg = iD
      img = toImageRGBA8 dImg
      trl = moveOriginBy (r2 (fromIntegral w / 2, fromIntegral h / 2 :: n)) mempty
      p   = rasterificPtTransf trl (R.V2 0 0)

-- Saving files --------------------------------------------------------

rasterBraille sz = renderDia Braille (BrailleOptions sz)

-- | Render a 'Braille' diagram to a file with the given size. The
--   format is determined by the extension (@.png@, @.tif@, @.bmp@, @.jpg@ and
--   @.pdf@ supported.
renderBraille :: TypeableFloat n => FilePath -> SizeSpec V2 n
              -> QDiagram Braille V2 n Any -> IO ()
renderBraille outFile spec d =
  case takeExtension outFile of
    _      -> writeBrl outFile brl
  where
    brl = renderDia Braille (BrailleOptions spec) d

writeBrl = writeFile

img2brl = img2brl' 8 f where
  f (PixelRGBA8 _ _ _ a) | a > 20 = True
  f _ = False

img2brl' dots c img = unlines $
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
  g y x True b a | c $ pixelAt img x y = setBit a b
  g _ _ _ _ a = a
  lineIndices = [0, (dots `div` 2) .. h - 1]
  columnIndices = [0, 2 .. w - 1]
  (h, w) = (imageHeight img, imageWidth img)

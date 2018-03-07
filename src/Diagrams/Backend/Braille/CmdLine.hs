{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- | Convenient creation of command-line-driven executables for
-- rendering diagrams to Braille.
--
-- * 'defaultMain' creates an executable which can render a single
--   diagram at various options.
--
-- * 'multiMain' is like 'defaultMain' but allows for a list of
--   diagrams from which the user can choose one to render.
--
-- * 'mainWith' is a generic form that does all of the above but with
--   a slightly scarier type.  See "Diagrams.Backend.CmdLine".  This
--   form can also take a function type that has a suitable final result
--   (any of arguments to the above types) and 'Parseable' arguments.
--
-- For a tutorial on command-line diagram creation see
-- <http://projects.haskell.org/diagrams/doc/cmdline.html>.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Braille.CmdLine (
    -- * General form of @main@
    -- $mainwith
    mainWith

    -- * Supported forms of @main@
  , defaultMain
  , multiMain

    -- * Backend tokens
  , Braille
  , B
  ) where

import           Diagrams.Backend.CmdLine
import           Diagrams.Prelude            hiding (height, interval, option,
                                                     output, width)
import           Diagrams.Backend.Braille

-- | 'mainWith' specialised to 'Diagram' 'Rasterific'.
defaultMain :: Diagram B -> IO ()
defaultMain = mainWith

instance TypeableFloat n => Mainable (QDiagram B V2 n Any) where
  type MainOpts (QDiagram B V2 n Any) = DiagramOpts

  mainRender opts d = chooseRender opts d

chooseRender :: TypeableFloat n => DiagramOpts -> QDiagram B V2 n Any -> IO () -- -> QDiagram Rasterific V2 n Any -> IO ()
chooseRender opts d | null path = putStr $ rasterBraille sz d
                    | otherwise = renderBraille path sz d
  where
    path = opts^.output
    sz   = fromIntegral <$> mkSizeSpec2D (opts^.width) (opts^.height)

-- | @multiMain@ takes a list of diagrams paired with names as input.
-- The generated executable then takes a @--selection@ option
-- specifying the name of the diagram that should be rendered.  The
-- list of available diagrams may also be printed by passing the
-- option @--list@.
multiMain :: [(String, Diagram B)] -> IO ()
multiMain = mainWith

instance TypeableFloat n => Mainable [(String, QDiagram B V2 n Any)] where
  type MainOpts [(String, QDiagram B V2 n Any)]
      = (MainOpts (QDiagram B V2 n Any), DiagramMultiOpts)

  mainRender = defaultMultiMainRender

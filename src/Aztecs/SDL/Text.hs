{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.SDL.Text
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.SDL.Text
  ( Font (..),
    Text (..),
    drawText,
    setup,
    load,
    draw,
  )
where

import Aztecs.Asset (Asset (..), Handle, lookupAsset)
import qualified Aztecs.Asset as Asset
import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Aztecs.SDL (Surface (..))
import Control.Arrow
import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import SDL hiding (Surface, Texture, Window, windowTitle)
import qualified SDL.Font as F

-- | Font asset.
--
-- @since 0.6
newtype Font = Font
  { -- | Unwrap the raw SDL font.
    --
    -- @since 0.6
    unFont :: F.Font
  }
  deriving (Eq, Show)

-- | @since 0.6
instance Asset Font where
  type AssetConfig Font = Int
  loadAsset fp size = Font <$> F.load fp size

-- | Text component.
--
-- @since 0.6
data Text = Text
  { -- | Text content.
    --
    -- @since 0.6
    textContent :: !T.Text,
    -- | Text font handle.
    --
    -- @since 0.6
    textFont :: !(Handle Font)
  }
  deriving (Eq, Show, Generic, NFData)

-- | @since 0.6
instance Component Text

-- | Draw text to a surface.
--
-- @since 0.6
drawText :: T.Text -> Font -> IO Surface
drawText content f = do
  !s <- F.solid (unFont f) (V4 255 255 255 255) content
  return Surface {sdlSurface = s, surfaceBounds = Nothing}

-- | Setup SDL TrueType-Font (TTF) support.
--
-- @since 0.6
setup :: (MonadAccess b m, MonadIO m) => m ()
setup = do
  liftIO F.initialize
  Asset.setup @_ @_ @Font

-- | Load font assets.
--
-- @since 0.6
load :: (MonadIO m, ArrowQuery m q, MonadSystem q s) => s ()
load = Asset.loadAssets @Font

-- | Draw text components.
--
-- @since 0.6
draw :: (ArrowDynamicQueryReader qr, ArrowQueryReader qr, MonadReaderSystem qr s, MonadIO s, MonadAccess b ma) => s (ma ())
draw = do
  !texts <-
    S.all
      ()
      ( proc () -> do
          e <- Q.entity -< ()
          t <- Q.fetch -< ()
          s <- Q.fetchMaybe -< ()
          returnA -< (e, t, s)
      )

  !assetServer <- S.single () Q.fetch
  let !textFonts =
        mapMaybe
          (\(eId, t, maybeSurface) -> (eId,textContent t,maybeSurface,) <$> lookupAsset (textFont t) assetServer)
          texts
  !draws <-
    mapM
      ( \(eId, content, maybeSurface, font) -> do
          case maybeSurface of
            Just lastSurface -> freeSurface $ sdlSurface lastSurface
            Nothing -> return ()
          surface <- liftIO $ drawText content font
          return (eId, surface)
      )
      textFonts
  let go (eId, surface) = A.insert eId $ bundle surface
  return $ mapM_ go draws

{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
import Control.Arrow (returnA)
import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import SDL hiding (Surface, Texture, Window, windowTitle)
import qualified SDL.Font as F

newtype Font = Font {unFont :: F.Font}
  deriving (Eq, Show)

instance Asset Font where
  type AssetConfig Font = Int
  loadAsset fp size = Font <$> F.load fp size

data Text = Text {textContent :: !T.Text, textFont :: !(Handle Font)}
  deriving (Eq, Show, Generic, NFData)

instance Component Text

drawText :: T.Text -> Font -> IO Surface
drawText content f = do
  !s <- F.solid (unFont f) (V4 255 255 255 255) content
  return Surface {sdlSurface = s, surfaceBounds = Nothing}

-- | Setup SDL TrueType-Font (TTF) support.
setup :: (MonadAccess b m, MonadIO m) => m ()
setup = do
  liftIO F.initialize
  Asset.setup @_ @_ @Font

-- | Load font assets.
load :: (MonadIO m, ArrowQuery m q, MonadSystem q s) => s ()
load = Asset.loadAssets @Font

-- | Draw text components.
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
  return $ mapM_ (uncurry A.insert) draws

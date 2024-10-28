module TUI.Utils where

import Brick.BChan
import Brick.Main
  ( App (..),
    customMainWithDefaultVty,
  )
import Brick.Types (Widget)
import Brick.Widgets.Core (str)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty
  ( Vty,
  )
import TUI.Service.Types
import TUI.Types

-- Creates a Brick application by providing an `TickEvent`
-- which is sent to the Brick application by a custom defined time interval
-- TODO: Extract to a (simple) library??
customMainWithInterval ::
  (Ord n, HasTickEvent e) =>
  -- | interval in microseconds
  Int ->
  -- | Custom event channel sending into Brick app
  Maybe (BChan e) ->
  -- | Brick application
  App s e n ->
  -- | Initial application state
  s ->
  IO (s, Vty)
customMainWithInterval ms mUserChan app initialAppState = do
  inCh <- case mUserChan of
    Nothing -> liftIO $ newBChan 10
    Just uc -> pure uc

  _ <- forkIO $ forever $ do
    writeBChan inCh tickEvent
    threadDelay ms

  customMainWithDefaultVty (Just inCh) app initialAppState

loadingString :: String
loadingString = "⣀"

loadingStr :: forall n. Widget n
loadingStr = str loadingString

emptyStr :: forall n. Widget n
emptyStr = str " "

getPriceByFiat :: Fiat -> Prices -> WPrice
getPriceByFiat fiat p = case fiat of
  FiatEUR -> WPrice (pEUR p)
  FiatUSD -> WPrice (pUSD p)
  FiatGBP -> WPrice (pGBP p)
  FiatCAD -> WPrice (pCAD p)
  FiatCHF -> WPrice (pCHF p)
  FiatAUD -> WPrice (pAUD p)
  FiatJPY -> WPrice (pJPY p)

type RgbColor = (Double, Double, Double)

type RgbaColor = (Double, Double, Double, Double)

applyAlpha :: Double -> RgbColor -> RgbaColor
applyAlpha alpha (r, g, b) = (r, g, b, alpha)

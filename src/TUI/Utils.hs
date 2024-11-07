{-# LANGUAGE DataKinds #-}

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

fps :: Int
fps = 60

maxFetchTick :: Int
maxFetchTick = 3 * 60 * fps -- 3min

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

emptyStr :: forall n. Widget n
emptyStr = str " "

-- | Helper to convert SATS to BTC
toBtc :: Amount SATS -> Amount BTC
-- To avoid floating-point precision issues using `Double`in `Amount`:
-- 1. Rounding and formatting result to 8 decimals
-- 2. `read` it back to `Amount` based on that decimals
toBtc (Amount a) = Amount $ read $ printBitcoinValue $ a / 100_000_000

-- | Helper to convert BTC to SATS
toSats :: Amount BTC -> Amount SATS
toSats (Amount a) =
  -- To avoid floating-point precision issues using `Double`in `Amount`:
  -- following extra steps are needed to remove decimal places for SATS:
  -- 1. Formatting without decimal
  -- 2. `read` it back to whole number
  Amount $ read $ printSatsValue (a * 100_000_000)

-- | Helper to convert Fiat to BTC
fiatToBtc :: forall (a :: Fiat). Amount a -> Price a -> Amount BTC
fiatToBtc (Amount a) (Price p) =
  -- To avoid floating-point precision issues using `Double`in `Amount`:
  -- 1. Rounding and formatting result to 8 decimals
  -- 2. `read` it back to `Amount` based on that decimals
  Amount $ read $ printBitcoinValue $ a / p

-- | Helper to convert BTC to Fiat
btcToFiat :: forall (a :: Fiat). Amount BTC -> Price a -> Amount a
-- To avoid floating-point precision issues using `Double`in `Amount`:
-- 1. Rounding and formatting result to two decimals
-- 2. `read` it back to `Amount` based on that decimals
btcToFiat (Amount b) (Price p) = Amount $ read $ printFiatValue $ b * p

-- | Helper to convert Fiat to Sats
fiatToSats :: forall (a :: Fiat). Amount a -> Price a -> Amount SATS
fiatToSats a p = toSats $ fiatToBtc a p

-- | Helper to convert SATS to Fiat
satsToFiat :: forall (a :: Fiat). Amount SATS -> Price a -> Amount a
satsToFiat s = btcToFiat (toBtc s)

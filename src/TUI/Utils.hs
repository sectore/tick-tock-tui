module TUI.Utils where

import Brick.BChan
import Brick.Main (
  App (..),
  customMainWithDefaultVty,
 )
import Brick.Types (Widget)
import Brick.Widgets.Core (str)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty (
  Vty,
 )
import TUI.Service.Types
import TUI.Types

fps :: Int
fps = 60

-- Creates a Brick application by providing an `TickEvent`
-- which is sent to the Brick application by a custom defined time interval
-- TODO: Extract to a (simple) library??
customMainWithInterval
  :: (Ord n, HasTickEvent e)
  => Int
  -- ^ interval in microseconds
  -> Maybe (BChan e)
  -- ^ Custom event channel sending into Brick app
  -> App s e n
  -- ^ Brick application
  -> s
  -- ^ Initial application state
  -> IO (s, Vty)
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

-- \| Helper to convert any `Amount` of `BTC` to `Fiat` by given `Price`
btcToFiat :: forall (a :: Fiat). Amount BTC -> Price a -> Amount a
-- To avoid floating-point precision issues using `Double`in `Amount`:
-- 1. Rounding and formatting result to two decimals
-- 2. `read` it back to `Amount` based on that decimals
btcToFiat (Amount b) (Price p) = Amount $ read $ printFiatValue $ b * p

-- | Helper to convert any `Amount` of `SATS` to `Fiat` by given `Price`
satsToFiat :: forall (a :: Fiat). Amount SATS -> Price a -> Amount a
satsToFiat s = btcToFiat (toBtc s)

-- | Helper to convert any `Amount` of `Fiat` to `BTC` by given `Price`
fiatToBtc :: forall (a :: Fiat). Amount a -> Price a -> Amount BTC
fiatToBtc (Amount a) (Price p) =
  -- To avoid floating-point precision issues using `Double`in `Amount`:
  -- 1. Rounding and formatting result to 8 decimals
  -- 2. `read` it back to `Amount` based on that decimals
  Amount $ read $ printBitcoinValue $ a / p

-- | Helper to convert any `Amount` of `Fiat` to `SATS` by given `Price`
fiatToSats :: forall (a :: Fiat). Amount a -> Price a -> Amount SATS
fiatToSats a p = toSats $ fiatToBtc a p

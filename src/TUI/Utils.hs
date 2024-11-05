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

loadingString :: String
loadingString = "⣀"

loadingStr :: forall n. Widget n
loadingStr = str loadingString

emptyStr :: forall n. Widget n
emptyStr = str " "

toBtc :: Amount SATS -> Amount BTC
toBtc (Amount a) = Amount $ a / 100_000_000

toSats :: Amount BTC -> Amount SATS
toSats (Amount a) = Amount $ a * 100000000

-- helper to convert Fiat to BTC
fiatToBtc :: forall (a :: Fiat). Amount a -> Price a -> Amount BTC
fiatToBtc (Amount a) (Price p) = Amount $ a / p

-- helper to convert Fiat to Sats
fiatToSats :: forall (a :: Fiat). Amount a -> Price a -> Amount SATS
fiatToSats a p = toSats $ fiatToBtc a p

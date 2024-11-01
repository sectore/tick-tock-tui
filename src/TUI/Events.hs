{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module TUI.Events where

import Brick.Main
  ( halt,
  )
import Brick.Types
  ( BrickEvent (..),
    EventM,
  )
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks)
import Data.Functor ((<&>))
import Graphics.Vty qualified as V
import Lens.Micro (Lens')
import Lens.Micro.Mtl
import TUI.Service.Types (ApiEvent (..), Bitcoin (..), RemoteData (..))
import TUI.Types
import TUI.Utils (maxFetchTick)

sendApiEvent :: ApiEvent -> AppEventM ()
sendApiEvent e = do
  ch <- asks outChan
  liftIO $ STM.atomically $ STM.writeTChan ch e

startEvent :: TChan ApiEvent -> EventM () TUIState ()
startEvent outCh =
  -- fetch all data at start
  runReaderT (sendApiEvent FetchAllData) (AppEventEnv outCh)

setLoading :: Lens' TUIState (RemoteData e a) -> AppEventM ()
setLoading lens = do
  mCurrent <-
    use lens <&> \case
      Success val -> Just val
      _ -> Nothing
  lens .= Loading mCurrent

appEvent :: TChan ApiEvent -> BrickEvent () TUIEvent -> EventM () TUIState ()
appEvent outCh e =
  runReaderT handleEvent (AppEventEnv outCh)
  where
    handleEvent = case e of
      VtyEvent ve -> handleKeyEvent ve
      AppEvent ae -> handleAppEvent ae
      _ -> return ()

handleKeyEvent :: V.Event -> AppEventM ()
handleKeyEvent e = do
  currentView' <- use currentView
  case e of
    V.EvKey (V.KChar '1') [] -> currentView .= PriceView
    V.EvKey (V.KChar '2') [] -> currentView .= FeesView
    V.EvKey (V.KChar '3') [] -> currentView .= BlockView
    V.EvKey (V.KChar '4') [] -> currentView .= ConverterView
    V.EvKey (V.KChar 'a') [] -> animate %= not
    V.EvKey (V.KChar 's') [] ->
      when (currentView' == PriceView) $
        selectedFiat %= next
      where
        next f
          | f == maxBound = minBound
          | otherwise = succ f
    V.EvKey (V.KChar 't') [] ->
      when (currentView' == PriceView) $
        selectedBitcoin %= toggle
      where
        toggle b
          | b == BTC = SATS
          | otherwise = BTC
    V.EvKey (V.KChar 'r') [] -> case currentView' of
      FeesView -> do
        fetchTick .= 0
        lastFetchTick .= 0
        setLoading fees
        -- fetch fees
        sendApiEvent FetchFees
      PriceView -> do
        fetchTick .= 0
        lastFetchTick .= 0
        setLoading prices
        -- fetch prices
        sendApiEvent FetchPrices
      BlockView -> do
        fetchTick .= 0
        lastFetchTick .= 0
        setLoading block
        -- fetch block data
        sendApiEvent FetchBlock
      _ -> return ()
    V.EvKey V.KEsc [] -> lift halt
    V.EvKey (V.KChar 'q') [] -> lift halt
    _ -> return ()

handleAppEvent :: TUIEvent -> AppEventM ()
handleAppEvent e = do
  case e of
    ev | ev == tickEvent -> do
      -- don't count `tick` endlessly, but set it back to 0 if > 216000 (1h at 60 FPS)
      -- just to save memory (not sure if it makes really sense ...)
      tick %= (`mod` 216001) . (+ 1)

      currentF <- use fetchTick
      lastF <- use lastFetchTick
      -- trigger reload data
      when (currentF - lastF >= maxFetchTick) $ do
        -- reset last fetch time
        lastFetchTick .= 0
        -- update loading states
        setLoading prices
        setLoading fees
        setLoading block
        -- load all data
        sendApiEvent FetchAllData

      fetchTick %= (`mod` (maxFetchTick + 1)) . (+ 1)
    PriceUpdated p -> do
      prices .= p
    FeesUpdated f -> do
      fees .= f
    BlockUpdated b -> do
      block .= b
    _ -> return ()

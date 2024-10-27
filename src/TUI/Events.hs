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
import Data.Functor ((<&>))
import Graphics.Vty qualified as V
import Lens.Micro (Lens')
import Lens.Micro.Mtl
import TUI.Service.Types (ApiEvent (..), Currency (..), RemoteData (..))
import TUI.Types

sendApiEvent :: TChan ApiEvent -> ApiEvent -> IO ()
sendApiEvent outCh e = STM.atomically $ STM.writeTChan outCh e

wFetchAllData :: TChan ApiEvent -> IO ()
wFetchAllData outCh = sendApiEvent outCh FetchAllData

wFetchFees :: TChan ApiEvent -> IO ()
wFetchFees outCh = sendApiEvent outCh FetchFees

wFetchPrices :: TChan ApiEvent -> IO ()
wFetchPrices outCh = sendApiEvent outCh FetchPrices

startEvent :: TChan ApiEvent -> EventM () TUIState ()
startEvent outCh = do
  -- fetch all data at start
  liftIO $ wFetchAllData outCh
  pure ()

setLoading :: Lens' TUIState (RemoteData e a) -> EventM () TUIState ()
setLoading lens = do
  mCurrent <-
    use lens <&> \case
      Success val -> Just val
      _ -> Nothing
  lens .= Loading mCurrent

appEvent :: TChan ApiEvent -> BrickEvent () TUIEvent -> EventM () TUIState ()
appEvent outCh e =
  case e of
    VtyEvent ve -> handleKeyEvent ve outCh
    AppEvent ae -> handleAppEvent ae outCh
    _ -> return ()

handleKeyEvent :: V.Event -> TChan ApiEvent -> EventM () TUIState ()
handleKeyEvent e outCh = do
  currentView' <- use currentView
  case e of
    V.EvKey (V.KChar '1') [] -> currentView .= FeesView
    V.EvKey (V.KChar '2') [] -> currentView .= PriceView
    V.EvKey (V.KChar '3') [] -> currentView .= BlockView
    V.EvKey (V.KChar '4') [] -> currentView .= ConverterView
    V.EvKey (V.KChar '5') [] -> currentView .= DraftView
    V.EvKey (V.KChar 't') [] -> when (currentView' == PriceView) $ do
      curr <- use selectedCurrency
      selectedCurrency .= if curr == USD then EUR else USD
    V.EvKey (V.KChar 'r') [] -> case currentView' of
      FeesView -> do
        setLoading fees
        liftIO $ wFetchFees outCh
      PriceView -> do
        setLoading prices
        liftIO $ wFetchPrices outCh
      _ -> return ()
    V.EvKey V.KEsc [] -> halt
    V.EvKey (V.KChar 'q') [] -> halt
    _ -> return ()

handleAppEvent :: TUIEvent -> TChan ApiEvent -> EventM n TUIState ()
handleAppEvent e outCh = do
  case e of
    ev | ev == tickEvent -> do
      -- count `tick` by 1, but don't count it endless.
      -- Set it back to 0 if `tick` > 216000 (1h at 60 FPS)
      tick %= (`mod` 216001) . (+ 1)
      -- tick %= (+ 1)
      currentTick <- use tick
      lastFetch <- use lastFetchTime
      -- 10800 ticks = 3min seconds at 60 FPS
      when (currentTick - lastFetch >= 10800) $ do
        -- set `Loading` price
        mCurrentPrice <-
          use prices <&> \case
            Success pr -> Just pr
            _ -> Nothing
        prices .= Loading mCurrentPrice
        -- set `Loading` fees
        mCurrentFees <-
          use fees <&> \case
            Success pr -> Just pr
            _ -> Nothing
        fees .= Loading mCurrentFees
        -- reset last fetch time
        lastFetchTime .= currentTick
        liftIO $ wFetchAllData outCh
    PriceUpdated p -> do
      prices .= p
    FeesUpdated f -> do
      fees .= f
    _ -> return ()

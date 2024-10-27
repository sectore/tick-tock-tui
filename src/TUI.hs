{-# LANGUAGE DataKinds #-}

module TUI where

import Brick.BChan
import Brick.Main
  ( App (..),
    showFirstCursor,
  )
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (newTChanIO)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad (forever)
import TUI.Attr (tuiAttrMap)
import TUI.Events (appEvent, startEvent)
import TUI.Service.Mempool qualified as M
import TUI.Service.Types
import TUI.Types
import TUI.Utils (customMainWithInterval)
import TUI.Widgets.App (drawApp)

run :: IO ()
run = do
  outCh <- newTChanIO
  -- \^ out channel to send messages out of TUI app
  inCh <- newBChan 10
  -- \^ in(to) channel to send messages into TUI app

  -- listen for messages outcoming from TUI app
  foreverId <- forkIO $ forever $ do
    e <- STM.atomically $ STM.readTChan outCh
    case e of
      FetchFees -> do
        _ <- M.fetchFees inCh
        pure ()
      FetchPrices -> do
        _ <- M.fetchPrices inCh
        pure ()
      FetchAllData -> do
        _ <- M.fetchPrices inCh
        _ <- M.fetchFees inCh
        pure ()

  -- run TUI app
  _ <- customMainWithInterval interval (Just inCh) (theApp outCh) initialState

  -- kill threads
  killThread foreverId
  where
    interval = 1_000_000 `div` 60 -- 60 FPS
    initialState :: TUIState
    initialState =
      TUIState
        { _currentView = FeesView,
          _tick = 0,
          _price = NotAsked,
          _fees = NotAsked,
          _lastFetchTime = 0,
          _selectedCurrency = EUR
        }

    theApp :: TChan ApiEvent -> App TUIState TUIEvent ()
    theApp outCh =
      App
        { appDraw = drawApp,
          appChooseCursor = showFirstCursor,
          appHandleEvent = appEvent outCh,
          appStartEvent = startEvent outCh,
          appAttrMap = const tuiAttrMap
        }

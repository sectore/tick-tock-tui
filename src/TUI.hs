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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..))
import TUI.Attr (tuiAttrMap)
import TUI.Config (Config (..), loadConfig)
import TUI.Events (appEvent, startEvent)
import TUI.Service.Mempool qualified as M
import TUI.Service.Types
import TUI.Types
import TUI.Utils (customMainWithInterval)
import TUI.Widgets.App (drawApp)

run :: IO ()
run = do
  -- config defined in .env
  config <- loadConfig
  -- out channel to send messages from TUI app
  outCh <- newTChanIO
  -- in channel to send messages into TUI app
  inCh <- newBChan 10
  let sEnv =
        ServiceEnv
          { envMempoolUrl = cfgMempoolUrl config,
            envInChan = inCh
          }
  -- listen for messages coming from TUI app
  foreverId <- forkIO $ flip runReaderT sEnv $ forever $ do
    e <- liftIO $ STM.atomically $ STM.readTChan outCh
    case e of
      FetchFees -> M.fetchFees
      FetchPrices -> M.fetchPrices
      FetchAllData -> M.fetchAllData

  -- run TUI app
  _ <- customMainWithInterval interval (Just inCh) (theApp outCh) initialState

  -- kill threads
  killThread foreverId
  where
    interval = 1_000_000 `div` 60 -- 60 FPS
    initialState :: TUIState
    initialState =
      TUIState
        { _currentView = PriceView,
          _tick = 0,
          _prices = NotAsked,
          _fees = NotAsked,
          _lastFetchTime = 0,
          _selectedFiat = EUR
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

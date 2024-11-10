{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module TUI where

import Brick (CursorLocation)
import Brick.BChan
import Brick.Focus (focusRingCursor)
import Brick.Forms (formFocus)
import Brick.Main
  ( App (..),
  )
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (newTChanIO)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..))
import Data.Time.LocalTime (getCurrentTimeZone)
import TUI.Attr (tuiAttrMap)
import TUI.Config (Config (..), loadConfig)
import TUI.Events (appEvent, startEvent)
import TUI.Service.Mempool qualified as M
import TUI.Service.Types
import TUI.Types
import TUI.Utils (customMainWithInterval, fps)
import TUI.Widgets.App (drawApp)
import TUI.Widgets.Converter (initialConverterData, mkConverterForm)

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
      FetchBlock -> M.fetchBlock
      FetchAllData -> M.fetchAllData

  initialState <-
    getCurrentTimeZone >>= \tz ->
      let initialFiat = USD
          initialBitcoin = BTC
       in pure
            TUIState
              { _timeZone = tz,
                _currentView = FeesView,
                _converterForm = mkConverterForm (initialConverterData initialFiat initialBitcoin),
                _prevConverterForm = Nothing,
                _animate = False,
                _extraInfo = False,
                _tick = 0,
                _fetchTick = 0,
                _lastFetchTick = 0,
                _prices = NotAsked,
                _fees = NotAsked,
                _block = NotAsked,
                _selectedFiat = initialFiat,
                _selectedBitcoin = initialBitcoin,
                _showMenu = False,
                _stLastBrickEvent = Nothing
              }
  -- run TUI app
  _ <- customMainWithInterval interval (Just inCh) (theApp outCh) initialState

  -- kill threads
  killThread foreverId
  where
    interval = 1_000_000 `div` fps -- 60 FPS
    chooseCursor :: TUIState -> [CursorLocation TUIResource] -> Maybe (CursorLocation TUIResource)
    chooseCursor TUIState {..} = chooseCursor' _currentView
      where
        chooseCursor' ConverterView = focusRingCursor formFocus _converterForm
        chooseCursor' _ = const Nothing
    theApp :: TChan ApiEvent -> App TUIState TUIEvent TUIResource
    theApp outCh =
      App
        { appDraw = drawApp,
          appChooseCursor = chooseCursor,
          appHandleEvent = appEvent outCh,
          appStartEvent = startEvent outCh,
          appAttrMap = const tuiAttrMap
        }

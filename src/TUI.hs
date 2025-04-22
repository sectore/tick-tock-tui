{-# LANGUAGE RecordWildCards #-}

module TUI where

import Brick (CursorLocation)
import Brick.BChan
import Brick.Focus (focusRingCursor)
import Brick.Forms (formFocus)
import Brick.Main (
  App (..),
 )
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (newTChanIO)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Time.LocalTime (getCurrentTimeZone)
import System.Directory (
  XdgDirectory (..),
  getXdgDirectory,
 )
import TUI.Attr (tuiAttrMap)
import TUI.Config (Config (..), getConfig)
import TUI.Events (appEvent, startEvent)
import qualified TUI.Service.Common as C
import qualified TUI.Service.Kraken as K
import qualified TUI.Service.Mempool as M
import TUI.Service.Types
import TUI.Storage (defaultStorage)
import qualified TUI.Storage as STG
import TUI.Types
import TUI.Utils (customMainWithInterval, fps)
import TUI.Widgets.App (drawApp)
import TUI.Widgets.Converter (initialConverterData, mkConverterForm)
import TUI.Widgets.Ratio (initialRatioData, mkRatioForm)

run :: IO ()
run = do
  -- get `Config` from args
  config <-
    getXdgDirectory XdgState "tick-tock-tui"
      >>= getConfig (MempoolUrl "https://mempool.space")
  storage <-
    -- by ignoring previous stored data, return default data.
    if cfgIgnoreStorage config
      then pure defaultStorage
      else do
        -- Try to get `TUIStorage` data from file or use default data
        STG.load (cfgStorageDirectory config)
          <&> fromMaybe defaultStorage

  -- out channel to send messages from TUI app
  outCh <- newTChanIO
  -- in channel to send messages into TUI app
  inCh <- newBChan 10
  let sEnv =
        ServiceEnv
          { envMempoolUrl = cfgMempoolUrl config
          , envInChan = inCh
          }

  -- TODO: get selected ticker from state or storage
  let initialTicker :: Ticker = mkTicker "ETH"
  -- listen for messages coming from TUI app
  foreverId <- forkIO $ flip runReaderT sEnv $ forever $ do
    e <- liftIO $ STM.atomically $ STM.readTChan outCh
    case e of
      FetchFees -> M.fetchFees
      FetchPrices -> M.fetchPrices
      FetchBlock -> M.fetchBlock
      FetchAssetPrice t -> K.fetchAssetPrice t
      FetchAllData ticker -> C.fetchAllData ticker

  initialState <-
    getCurrentTimeZone >>= \tz ->
      let initialFiat = stgSelectedFiat storage
          initialBitcoin = stgSelectedBitcoin storage
          initialBtcAmount = stgBtcAmount storage
       in pure
            TUIState
              { timeZone' = tz
              , _currentView = stgCurrentView storage
              , _converterForm = mkConverterForm $ initialConverterData initialFiat initialBitcoin initialBtcAmount
              , _prevConverterForm = Nothing
              , _ratioForm = mkRatioForm $ initialRatioData initialTicker
              , _prevRatioForm = Nothing
              , _animate = stgAnimate storage
              , _extraInfo = stgExtraInfo storage
              , _tick = 0
              , _fetchTick = 0
              , _lastFetchTick = 0
              , maxFetchTick' = cfgReloadInterval config * fps -- seconds -> fps
              , _prices = NotAsked
              , _fees = NotAsked
              , _block = NotAsked
              , _assetPrice = NotAsked
              , _selectedFiat = initialFiat
              , _selectedBitcoin = initialBitcoin
              , _showMenu = stgShowMenu storage
              }
  -- run TUI app
  (lastState, _) <-
    customMainWithInterval interval (Just inCh) (theApp outCh config initialTicker) initialState

  -- persistant parts of `TUIState`
  _ <- liftIO $ STG.save (STG.toStorage lastState) (cfgStorageDirectory config)
  -- kill threads
  killThread foreverId
  where
    interval = 1_000_000 `div` fps -- 60 FPS
    chooseCursor :: TUIState -> [CursorLocation TUIResource] -> Maybe (CursorLocation TUIResource)
    chooseCursor TUIState{..} = chooseCursor' _currentView
      where
        chooseCursor' ConverterView = focusRingCursor formFocus _converterForm
        chooseCursor' RatioView = focusRingCursor formFocus _ratioForm
        chooseCursor' _ = const Nothing
    theApp :: TChan ApiEvent -> Config -> Ticker -> App TUIState TUIEvent TUIResource
    theApp outCh conf ticker =
      App
        { appDraw = drawApp conf
        , appChooseCursor = chooseCursor
        , appHandleEvent = appEvent outCh
        , appStartEvent = startEvent ticker outCh
        , appAttrMap = const tuiAttrMap
        }

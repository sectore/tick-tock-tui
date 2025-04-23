module TUI.Events where

import Brick.Focus (focusGetCurrent)
import Brick.Forms (
  Form (..),
  formState,
  handleFormEvent,
  invalidFields,
  updateFormState,
 )
import Brick.Main (
  halt,
 )
import Brick.Types (
  BrickEvent (..),
  EventM,
 )
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks)
import Control.Monad.State.Strict (MonadState)
import Data.Char (toUpper)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro (Lens', (%~), (&), (.~), (^.))
import Lens.Micro.Mtl
import TUI.Service.Types (
  ApiEvent (..),
  Bitcoin (..),
  Fiat (..),
  Prices (..),
  RemoteData (..),
  Ticker,
  mkTicker,
  pAUD,
  pJPY,
  tickerToString,
  unTicker,
 )
import TUI.Types
import TUI.Utils (btcToFiat, fiatToBtc, satsToFiat, toBtc, toSats)
import TUI.Widgets.Converter (mkConverterForm)

sendApiEvent :: ApiEvent -> AppEventM ()
sendApiEvent e = do
  ch <- asks outChan
  liftIO $ STM.atomically $ STM.writeTChan ch e

startEvent :: Ticker -> TChan ApiEvent -> EventM TUIResource TUIState ()
startEvent ticker outCh =
  -- fetch all data at start
  runReaderT (sendApiEvent $ FetchAllData ticker) (AppEventEnv outCh)

setLoading :: Lens' TUIState (RemoteData e a) -> AppEventM ()
setLoading lens = do
  mCurrent <-
    use lens <&> \case
      Success val -> Just val
      _ -> Nothing
  lens .= Loading mCurrent

updateConversion :: (MonadState TUIState m) => Maybe TUIResource -> m ()
updateConversion focusedField = do
  use prices >>= \case
    Success ps -> do
      cf <- use converterForm
      case focusedField of
        Just ConverterBtcField -> updateBtcBased ps cf
        Just ConverterSatField -> updateSatBased ps cf
        Just ConverterFiatField -> updateFiatBased ps cf
        -- ignore fields from other views
        Just _ -> pure ()
        Nothing -> updateFiatBased ps cf
      -- update previous state
      use converterForm >>= \v -> prevConverterForm .= Just v
    _ -> pure ()
  where
    updateBtcBased :: (MonadState TUIState m) => Prices -> ConverterForm -> m ()
    updateBtcBased ps cf =
      let st = formState cf
          newBtcAmount = st ^. cdBTC
       in converterForm
            .= updateFormState
              ( st
                  & cdUsd
                    .~ btcToFiat newBtcAmount (pUSD ps)
                  & cdCAD
                    .~ btcToFiat newBtcAmount (pCAD ps)
                  & cdEUR
                    .~ btcToFiat newBtcAmount (pEUR ps)
                  & cdGBP
                    .~ btcToFiat newBtcAmount (pGBP ps)
                  & cdAUD
                    .~ btcToFiat newBtcAmount (pAUD ps)
                  & cdJPY
                    .~ btcToFiat newBtcAmount (pJPY ps)
                  & cdCHF
                    .~ btcToFiat newBtcAmount (pCHF ps)
                  & cdSATS
                    .~ toSats newBtcAmount
              )
              cf

    updateSatBased :: (MonadState TUIState m) => Prices -> ConverterForm -> m ()
    updateSatBased ps cf =
      let st = formState cf
          newSatsAmount = st ^. cdSATS
       in converterForm
            .= updateFormState
              ( st
                  & cdUsd
                    .~ satsToFiat newSatsAmount (pUSD ps)
                  & cdCAD
                    .~ satsToFiat newSatsAmount (pCAD ps)
                  & cdEUR
                    .~ satsToFiat newSatsAmount (pEUR ps)
                  & cdGBP
                    .~ satsToFiat newSatsAmount (pGBP ps)
                  & cdAUD
                    .~ satsToFiat newSatsAmount (pAUD ps)
                  & cdJPY
                    .~ satsToFiat newSatsAmount (pJPY ps)
                  & cdCHF
                    .~ satsToFiat newSatsAmount (pCHF ps)
                  & cdBTC
                    .~ toBtc newSatsAmount
              )
              cf

    updateFiatBased :: (MonadState TUIState m) => Prices -> ConverterForm -> m ()
    updateFiatBased ps cf =
      let st = formState cf
          newBtcAmount = case st ^. cdSelectedFiat of
            EUR -> fiatToBtc (st ^. cdEUR) (pEUR ps)
            CAD -> fiatToBtc (st ^. cdCAD) (pCAD ps)
            AUD -> fiatToBtc (st ^. cdAUD) (pAUD ps)
            JPY -> fiatToBtc (st ^. cdJPY) (pJPY ps)
            GBP -> fiatToBtc (st ^. cdGBP) (pGBP ps)
            CHF -> fiatToBtc (st ^. cdCHF) (pCHF ps)
            USD -> fiatToBtc (st ^. cdUsd) (pUSD ps)
       in converterForm
            .= updateFormState
              ( st
                  & cdBTC
                    .~ newBtcAmount
                  & cdSATS
                    .~ toSats newBtcAmount
              )
              cf

appEvent :: TChan ApiEvent -> BrickEvent TUIResource TUIEvent -> EventM TUIResource TUIState ()
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
  cf <- use converterForm
  rf <- use ratioForm
  case e of
    -- Action: quit app (two ways)
    V.EvKey (V.KChar 'q') [V.MCtrl] -> lift halt
    V.EvKey (V.KChar 'q') [] -> lift halt
    -- Special case for `ConverterView`:
    -- In case of a validation error an user needs still to type any chars (eg. sat)
    -- but without getting in conflict with other `KChar` events (to navigate etc.)
    ev@(V.EvKey (V.KChar _) [])
      | currentView' == ConverterView
          && not (null (invalidFields cf)) ->
          lift $ zoom converterForm $ handleFormEvent (VtyEvent ev)
    -- Special case for `RatioView`:
    -- TODO: Refactor to check focus or edit mode
    ev@(V.EvKey (V.KChar _) [])
      | currentView' == RatioView
          && (not (null (invalidFields cf)) || (T.length (unTicker $ formState rf ^. rdTicker) <= 3)) ->
          lift $ zoom ratioForm $ handleFormEvent (VtyEvent ev)
    -- Action: navigate screens
    V.EvKey (V.KChar 'f') [] -> currentView .= FeesView
    V.EvKey (V.KChar 'b') [] -> currentView .= BlockView
    V.EvKey (V.KChar 'c') [] -> currentView .= ConverterView
    V.EvKey (V.KChar 'r') [] -> currentView .= RatioView
    -- Action: toggle animation
    V.EvKey (V.KChar 'a') [] -> animate %= not
    -- Action: toggle menu
    V.EvKey (V.KChar 'm') [] -> showMenu %= not
    V.EvKey V.KDown [] -> showMenu .= False
    V.EvKey V.KUp [] -> showMenu .= True
    -- Action: toggle fiat
    V.EvKey (V.KChar 't') [] -> do
      sf <- selectedFiat <%= next
      -- Update `formState` with current selected `Fiat`
      let updatedState = formState cf & cdSelectedFiat .~ sf
      -- and rebuild the form with it
      converterForm .= mkConverterForm updatedState
      updateConversion (focusGetCurrent $ formFocus cf)
      where
        next f
          | f == maxBound = minBound
          | otherwise = succ f
    -- Action: switch btc <-> sat
    V.EvKey (V.KChar 's') [] -> do
      sb <- selectedBitcoin <%= switch
      -- Update `formState` with current selected `Bitcoin`
      let updatedState = formState cf & cdSelectedBitcoin .~ sb
      -- and recreate the form with it
      converterForm .= mkConverterForm updatedState
      updateConversion (focusGetCurrent $ formFocus cf)
      where
        switch b
          | b == BTC = SATS
          | otherwise = BTC
    -- Action: reload data
    V.EvKey (V.KChar 'r') [V.MCtrl] -> do
      -- reset fetch ticks
      fetchTick .= 0
      lastFetchTick .= 0
      setLoading prices
      sendApiEvent FetchPrices
      case currentView' of
        FeesView -> do
          setLoading fees
          sendApiEvent FetchFees
        BlockView -> do
          setLoading block
          sendApiEvent FetchBlock
        ConverterView -> pure ()
        RatioView -> do
          setLoading assetPrice
          sendApiEvent $ FetchAssetPrice $ formState rf ^. rdTicker
    -- Action: toggle extra info
    V.EvKey (V.KChar 'e') [] -> extraInfo %= not
    -- all the other events
    -- `ConverterView` only
    ev | currentView' == ConverterView -> do
      let currentField = focusGetCurrent $ formFocus cf
      lift $ zoom converterForm $ handleFormEvent (VtyEvent ev)
      case ev of
        V.EvKey V.KEnter [] -> updateConversion currentField
        V.EvKey (V.KChar '\t') [] -> updateConversion currentField
        V.EvKey V.KBackTab [] -> updateConversion currentField
        V.EvKey V.KEsc [] ->
          -- Restore converterForm with its previous state
          use prevConverterForm
            >>= traverse_ (converterForm .=)
        _ -> pure ()
    -- `RatioView` only
    ev | currentView' == RatioView -> do
      lift $ zoom ratioForm $ handleFormEvent (VtyEvent ev)
      case ev of
        -- call API for valid data only
        V.EvKey V.KEnter [] | null (invalidFields rf) -> do
          setLoading assetPrice
          -- update form state to have current ticker value always in uppercase
          ratioForm %= \rf' -> updateFormState (formState rf' & rdTicker %~ (mkTicker . map toUpper . tickerToString)) rf'
          sendApiEvent $ FetchAssetPrice $ formState rf ^. rdTicker
        V.EvKey V.KEsc [] ->
          -- Restore ratioForm with its previous state
          use prevRatioForm
            >>= traverse_ (ratioForm .=)
        _ -> pure ()
    _ -> pure ()

handleAppEvent :: TUIEvent -> AppEventM ()
handleAppEvent e = do
  case e of
    ev | ev == tickEvent -> do
      -- don't count `tick` endlessly, but set it back to 0 if > 216000 (1h at 60 FPS)
      -- just to save memory (not sure if it makes really sense ...)
      tick %= (`mod` 216001) . (+ 1)

      currentF <- use fetchTick
      lastF <- use lastFetchTick
      maxF <- use maxFetchTick
      rf <- use ratioForm
      -- trigger reload data
      when (currentF - lastF >= maxF) $ do
        -- reset last fetch time
        lastFetchTick .= 0
        -- update loading states
        setLoading prices
        setLoading fees
        setLoading block
        setLoading assetPrice
        -- load all data
        sendApiEvent $ FetchAllData $ formState rf ^. rdTicker

      fetchTick %= (`mod` (maxF + 1)) . (+ 1)
    PriceUpdated p -> do
      prices .= p
      cf <- use converterForm
      let currentField = focusGetCurrent $ formFocus cf
      updateConversion currentField
    FeesUpdated f -> do
      fees .= f
    BlockUpdated b -> do
      block .= b
    AssetPriceUpdated _ p -> do
      assetPrice .= p
    _ -> pure ()

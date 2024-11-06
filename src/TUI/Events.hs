{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module TUI.Events where

import Brick.Focus (focusGetCurrent)
import Brick.Forms
  ( Form (..),
    formState,
    handleFormEvent,
    updateFormState,
  )
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
import Control.Monad.State.Strict (MonadState)
import Data.Functor ((<&>))
import Graphics.Vty qualified as V
import Lens.Micro (Lens', (&), (.~), (^.))
import Lens.Micro.Mtl
import TUI.Service.Types (ApiEvent (..), Bitcoin (..), Prices (pUSD), RemoteData (..))
import TUI.Types
import TUI.Utils (btcToFiat, fiatToBtc, maxFetchTick, satsToFiat, toBtc, toSats)

sendApiEvent :: ApiEvent -> AppEventM ()
sendApiEvent e = do
  ch <- asks outChan
  liftIO $ STM.atomically $ STM.writeTChan ch e

startEvent :: TChan ApiEvent -> EventM TUIResource TUIState ()
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

updateConversion :: (MonadState TUIState m) => Maybe TUIResource -> m ()
updateConversion focusedField = do
  use prices >>= \case
    Success ps -> do
      cf <- use converterForm
      let cs = formState cf
      case focusedField of
        Just ConverterBtcField -> updateBtcBased ps cf cs
        Just ConverterSatField -> updateSatBased ps cf cs
        Just ConverterFiatField -> updateFiatBased ps cf cs
        Nothing -> updateFiatBased ps cf cs
    _ -> pure ()
  where
    updateBtcBased :: (MonadState TUIState m) => Prices -> ConverterForm -> ConverterData -> m ()
    updateBtcBased ps cf cs =
      let newBtcAmount = cs ^. btcAmount
       in converterForm
            .= updateFormState
              ( cs
                  & fiatAmount .~ btcToFiat newBtcAmount (pUSD ps)
                  & satsAmount .~ toSats newBtcAmount
              )
              cf

    updateSatBased :: (MonadState TUIState m) => Prices -> ConverterForm -> ConverterData -> m ()
    updateSatBased ps cf cs =
      let newSatsAmount = cs ^. satsAmount
       in converterForm
            .= updateFormState
              ( cs
                  & fiatAmount .~ satsToFiat newSatsAmount (pUSD ps)
                  & btcAmount .~ toBtc newSatsAmount
              )
              cf

    updateFiatBased :: (MonadState TUIState m) => Prices -> ConverterForm -> ConverterData -> m ()
    updateFiatBased ps cf cs =
      let newBtcAmount = fiatToBtc (cs ^. fiatAmount) (pUSD ps)
       in converterForm
            .= updateFormState
              ( cs
                  & btcAmount .~ newBtcAmount
                  & satsAmount .~ toSats newBtcAmount
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

  case e of
    V.EvKey (V.KChar 'p') [] -> currentView .= PriceView
    V.EvKey (V.KChar 'f') [] -> currentView .= FeesView
    V.EvKey (V.KChar 'b') [] -> currentView .= BlockView
    V.EvKey (V.KChar 'c') [] -> currentView .= ConverterView
    V.EvKey (V.KChar 'a') [] -> animate %= not
    V.EvKey (V.KChar 's') [] ->
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
      PriceView -> fetchPrices
      ConverterView -> fetchPrices
      BlockView -> do
        fetchTick .= 0
        lastFetchTick .= 0
        setLoading block
        -- fetch block data
        sendApiEvent FetchBlock
    V.EvKey V.KEsc [] -> lift halt
    V.EvKey (V.KChar 'q') [] -> lift halt
    otherEv -> do
      stLastBrickEvent .= Just (VtyEvent otherEv)
      case currentView' of
        ConverterView -> do
          cf <- use converterForm
          let currentField = focusGetCurrent $ formFocus cf
          lift $ zoom converterForm $ handleFormEvent (VtyEvent otherEv)
          case otherEv of
            V.EvKey V.KEnter [] -> updateConversion currentField
            V.EvKey (V.KChar '\t') [] -> updateConversion currentField
            V.EvKey V.KBackTab [] -> updateConversion currentField
            _ -> pure ()
        _ -> pure ()
  where
    fetchPrices = do
      fetchTick .= 0
      lastFetchTick .= 0
      setLoading prices
      -- fetch prices
      sendApiEvent FetchPrices

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
      cf <- use converterForm
      let currentField = focusGetCurrent $ formFocus cf
      updateConversion currentField
    FeesUpdated f -> do
      fees .= f
    BlockUpdated b -> do
      block .= b
    _ -> pure ()

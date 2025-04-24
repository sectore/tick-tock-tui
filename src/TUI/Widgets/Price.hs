module TUI.Widgets.Price (drawPrice) where

import Brick.Types (
  Widget,
 )
import Brick.Widgets.Core
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (
  Amount (..),
  Bitcoin (..),
  Fiat (..),
  Prices (..),
  PricesRD,
  RemoteData (..),
 )
import TUI.Types (
  TUIResource (..),
  TUIState (..),
  extraInfo,
  prices,
  selectedBitcoin,
  selectedFiat,
  tick,
  timeZone,
 )
import TUI.Utils (btcToFiat, emptyStr, formatLocalTime, satsToFiat)
import TUI.Widgets.Loader (drawLoadingString4, drawSpinner)

drawPrice :: TUIState -> Widget TUIResource
drawPrice st =
  hBox
    [ if st ^. extraInfo then padLeft (Pad 2) $ rdToTimeStr rdPrices else emptyWidget
    , (if st ^. extraInfo then withBold else id) $
        padRight (Pad 1) loadingAnimation
          <+> btcStr
          <+> padLeftRight 1 (str "=")
          <+> rdToStr rdPrices
    ]
  where
    rdPrices :: PricesRD
    rdPrices = st ^. prices
    loadingAnimation =
      let spinner = drawSpinner (st ^. tick)
       in case rdPrices of
            Loading _ -> spinner
            _ -> emptyStr
    btcSelected = st ^. selectedBitcoin == BTC
    sAmount :: Amount SATS
    sAmount = Amount 1000
    bAmount :: Amount BTC
    bAmount = Amount 1
    btcStr = str $ if btcSelected then "1 BTC" else show sAmount
    priceStr :: Prices -> Widget n
    priceStr =
      let toPriceStr price =
            str . show $
              if btcSelected
                then btcToFiat bAmount price
                else satsToFiat sAmount price
       in case st ^. selectedFiat of
            EUR -> toPriceStr . pEUR
            USD -> toPriceStr . pUSD
            GBP -> toPriceStr . pGBP
            CAD -> toPriceStr . pCAD
            CHF -> toPriceStr . pCHF
            AUD -> toPriceStr . pAUD
            JPY -> toPriceStr . pJPY

    rdToStr rd =
      let loadingStr = drawLoadingString4 (st ^. tick)
       in case rd of
            NotAsked -> loadingStr
            Loading Nothing -> loadingStr
            Loading (Just p) -> priceStr p
            Failure _ -> withError $ str "error"
            Success p -> priceStr p

    rdToTimeStr :: PricesRD -> Widget n
    rdToTimeStr rd =
      let loadingStr = drawLoadingString4 (st ^. tick)
          timeStr :: Prices -> Widget n
          timeStr ps = str $ formatLocalTime (st ^. timeZone) (pTime ps)
       in case rd of
            NotAsked -> loadingStr
            Loading Nothing -> loadingStr
            Loading (Just ps) -> timeStr ps
            Failure _ -> withError $ str "error"
            Success ps -> timeStr ps

{-# LANGUAGE DataKinds #-}

module TUI.Widgets.Price (drawPrice) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
import Lens.Micro ((^.))
import TUI.Attr (withError)
import TUI.Service.Types (Amount (..), Bitcoin (..), Fiat (..), Price (..), Prices (..), RemoteData (..))
import TUI.Types (TUIResource (..), TUIState (..), prices, selectedBitcoin, selectedFiat, tick)
import TUI.Utils (btcToFiat, emptyStr, satsToFiat)
import TUI.Widgets.Loader (drawLoadingString4, drawSpinner)

drawPrice :: TUIState -> Widget TUIResource
drawPrice st =
  padRight (Pad 1) loadingAnimation
    <+> btcStr
    <+> padLeftRight 1 (str "=")
    <+> rdToStr rdPrices
  where
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
    btcStr = str $ if btcSelected then "BTC 1" else show sAmount
    calcP :: Price a -> Amount a
    calcP p =
      if btcSelected
        then btcToFiat bAmount p
        else satsToFiat sAmount p
    priceStr :: Prices -> Widget n
    priceStr = case st ^. selectedFiat of
      EUR -> str . show . calcP . pEUR
      USD -> str . show . calcP . pUSD
      GBP -> str . show . calcP . pGBP
      CAD -> str . show . calcP . pCAD
      CHF -> str . show . calcP . pCHF
      AUD -> str . show . calcP . pAUD
      JPY -> str . show . calcP . pJPY

    rdToStr rd =
      let loadingStr = drawLoadingString4 (st ^. tick)
       in case rd of
            NotAsked -> loadingStr
            Loading mp -> maybe loadingStr priceStr mp
            Failure _ -> withError $ str "error"
            Success p -> priceStr p

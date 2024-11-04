{-# LANGUAGE DataKinds #-}

module TUI.Widgets.Price (drawPrice) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (Amount (..), Bitcoin (..), Fiat (..), Price (..), Prices (..), RemoteData (..))
import TUI.Types (TUIResource (..), TUIState (..), prices, selectedBitcoin, selectedFiat, tick)
import TUI.Utils (emptyStr, loadingStr, toBtc)
import TUI.Widgets.Loader (drawSpinner)

drawPrice :: TUIState -> Widget TUIResource
drawPrice st =
  vBox
    [ hCenter $ padBottom (Pad 2) $ withBold $ str "Price " <+> loadingAnimation,
      hCenter $
        renderTable $
          surroundingBorder False $
            rowBorders False $
              columnBorders False $
                setDefaultColAlignment AlignLeft $
                  table
                    [ [ padRight (Pad 10) $ withBold $ btcStr,
                        padLeft (Pad 10) $ rdToStr rdPrices
                      ]
                    ]
    ]
  where
    rdPrices = st ^. prices
    sFiat = st ^. selectedFiat
    sBitcoin = st ^. selectedBitcoin
    btcSelected = sBitcoin == BTC
    sAmount :: Amount SATS
    sAmount = Amount 1000
    btcStr = str $ if btcSelected then "1 B" else show sAmount
    loadingAnimation =
      let spinner = drawSpinner (st ^. tick)
       in case rdPrices of
            NotAsked -> spinner
            Loading _ -> spinner
            _ -> emptyStr
    calcP :: Price a -> Price a
    calcP p@(Price v) =
      if btcSelected
        then p
        else Price (v * unAmount (toBtc sAmount))
    priceStr = case sFiat of
      EUR -> str . show . calcP . pEUR
      USD -> str . show . calcP . pUSD
      GBP -> str . show . calcP . pGBP
      CAD -> str . show . calcP . pCAD
      CHF -> str . show . calcP . pCHF
      AUD -> str . show . calcP . pAUD
      JPY -> str . show . calcP . pJPY

    rdToStr rd = case rd of
      NotAsked -> loadingStr
      Loading mp -> maybe loadingStr priceStr mp
      Failure _ -> withError $ str "error"
      Success p -> priceStr p

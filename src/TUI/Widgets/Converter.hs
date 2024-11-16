{-# LANGUAGE DataKinds #-}

module TUI.Widgets.Converter
  ( drawConverter,
    ConverterData (..),
    mkConverterForm,
    initialConverterData,
  )
where

import Brick
import Brick.Forms
  ( editShowableField,
    newForm,
    renderForm,
  )
import Brick.Widgets.Center
import Lens.Micro ((^.))
import TUI.Attr (withBold)
import TUI.Service.Types (Amount (..), Bitcoin (..), Fiat (..), RemoteData (Loading, NotAsked))
import TUI.Types
  ( ConverterData (..),
    ConverterForm,
    TUIResource (..),
    TUIState,
    cdAUD,
    cdBTC,
    cdCAD,
    cdCHF,
    cdEUR,
    cdGBP,
    cdJPY,
    cdSATS,
    cdSelectedBitcoin,
    cdSelectedFiat,
    cdUsd,
    converterForm,
    prices,
    tick,
  )
import TUI.Utils (emptyStr)
import TUI.Widgets.Loader (drawSpinner)

initialConverterData :: Fiat -> Bitcoin -> ConverterData
initialConverterData initialFiat initialBitcoin =
  ConverterData
    { _cdSelectedFiat = initialFiat,
      _cdSelectedBitcoin = initialBitcoin,
      _cdUsd = Amount 21,
      _cdGBP = Amount 21,
      _cdCAD = Amount 21,
      _cdCHF = Amount 21,
      _cdAUD = Amount 21,
      _cdEUR = Amount 21,
      _cdJPY = Amount 21,
      _cdBTC = Amount 0,
      _cdSATS = Amount 0
    }

mkConverterForm :: ConverterData -> ConverterForm
mkConverterForm cd =
  newForm
    [ fiatField,
      bitcoinField
    ]
    cd
  where
    fiatField = case cd ^. cdSelectedFiat of
      EUR -> editShowableField cdEUR ConverterFiatField
      CAD -> editShowableField cdCAD ConverterFiatField
      GBP -> editShowableField cdGBP ConverterFiatField
      AUD -> editShowableField cdAUD ConverterFiatField
      CHF -> editShowableField cdCHF ConverterFiatField
      JPY -> editShowableField cdJPY ConverterFiatField
      USD -> editShowableField cdUsd ConverterFiatField
    bitcoinField = case cd ^. cdSelectedBitcoin of
      BTC -> editShowableField cdBTC ConverterBtcField
      SATS -> editShowableField cdSATS ConverterSatField

drawConverter :: TUIState -> Widget TUIResource
drawConverter st =
  hCenter $
    vBox
      [ padBottom (Pad 2) $ hCenter $ withBold $ str "CONVERTER" <+> padLeft (Pad 1) loadingAnimation,
        padTopBottom 1 $ hCenter $ hLimit 20 $ renderForm (st ^. converterForm)
      ]
  where
    loadingAnimation =
      let spinner = drawSpinner (st ^. tick)
       in case st ^. prices of
            NotAsked -> spinner
            Loading _ -> spinner
            _ -> emptyStr

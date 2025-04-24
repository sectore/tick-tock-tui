module TUI.Widgets.Converter (
  drawConverter,
  mkConverterForm,
  initialConverterData,
)
where

import Brick
import Brick.Forms (
  Form (formState),
  editShowableField,
  newForm,
  renderForm,
 )
import Brick.Widgets.Center
import Lens.Micro ((^.))
import TUI.Attr (withBold)
import TUI.Service.Types (Amount (..), Bitcoin (..), Fiat (..), RemoteData (Loading, NotAsked))
import TUI.Types (
  ConverterData (..),
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
  editMode,
  prices,
  tick,
 )
import TUI.Utils (emptyStr, toSats)
import TUI.Widgets.Loader (drawSpinner)

initialConverterData :: Fiat -> Bitcoin -> Amount BTC -> ConverterData
initialConverterData selectedFiat selectedBitcoin btcAmount =
  ConverterData
    { _cdSelectedFiat = selectedFiat
    , _cdSelectedBitcoin = selectedBitcoin
    , _cdUsd = Amount 0
    , _cdGBP = Amount 0
    , _cdCAD = Amount 0
    , _cdCHF = Amount 0
    , _cdAUD = Amount 0
    , _cdEUR = Amount 0
    , _cdJPY = Amount 0
    , _cdBTC = btcAmount
    , _cdSATS = toSats btcAmount
    }

mkConverterForm :: ConverterData -> ConverterForm
mkConverterForm cd =
  newForm
    [ bitcoinField
    , fiatField
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

-- Renders a simple `str` to simulate a disabled form (no input visible, just text)
renderDisabledForm :: ConverterData -> Widget n
renderDisabledForm cd =
  vBox
    [ str bitcoinStr
    , str fiatStr
    ]
  where
    fiatStr = case cd ^. cdSelectedFiat of
      EUR -> show $ cd ^. cdEUR
      CAD -> show $ cd ^. cdCAD
      GBP -> show $ cd ^. cdGBP
      AUD -> show $ cd ^. cdAUD
      CHF -> show $ cd ^. cdCHF
      JPY -> show $ cd ^. cdJPY
      USD -> show $ cd ^. cdUsd
    bitcoinStr = case cd ^. cdSelectedBitcoin of
      BTC -> show $ cd ^. cdBTC
      SATS -> show $ cd ^. cdSATS

drawConverter :: TUIState -> Widget TUIResource
drawConverter st =
  vBox
    [ padBottom (Pad 2) $ hCenter $ withBold $ str "CONVERTER" <+> padLeft (Pad 1) loadingAnimation
    , padTopBottom 1 $
        hCenter $
          hLimit hSize $
            if st ^. editMode
              then renderForm cf
              else renderDisabledForm $ formState cf
    ]
  where
    cf = st ^. converterForm
    hSize =
      let cd = formState (st ^. converterForm)
          fiatL = case cd ^. cdSelectedFiat of
            EUR -> length $ show $ cd ^. cdEUR
            AUD -> length $ show $ cd ^. cdAUD
            CAD -> length $ show $ cd ^. cdCAD
            GBP -> length $ show $ cd ^. cdGBP
            CHF -> length $ show $ cd ^. cdCHF
            JPY -> length $ show $ cd ^. cdJPY
            USD -> length $ show $ cd ^. cdUsd
          bitcoinL = case cd ^. cdSelectedBitcoin of
            BTC -> length $ show $ cd ^. cdBTC
            SATS -> length $ show $ cd ^. cdSATS
       in max fiatL bitcoinL + 1

    loadingAnimation =
      let spinner = drawSpinner (st ^. tick)
       in case st ^. prices of
            NotAsked -> spinner
            Loading _ -> spinner
            _ -> emptyStr

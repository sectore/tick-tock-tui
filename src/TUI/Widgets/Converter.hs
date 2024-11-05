{-# LANGUAGE DataKinds #-}

module TUI.Widgets.Converter
  ( drawConverter,
    ConverterData (..),
    mkConverterForm,
    initialConverterData,
  )
where

import Brick
import Brick.Focus (focusGetCurrent)
import Brick.Forms
  ( Form (..),
    editShowableField,
    newForm,
    renderForm,
  )
import Brick.Widgets.Center
import Lens.Micro ((^.))
import TUI.Attr (withBold)
import TUI.Service.Types (Amount (..), RemoteData (Loading, NotAsked))
import TUI.Types (ConverterData (..), ConverterForm, TUIResource (..), TUIState, btcAmount, converterForm, fiatAmount, prices, satsAmount, selectedFiat, stLastBrickEvent, tick)
import TUI.Utils (emptyStr)
import TUI.Widgets.Loader (drawSpinner)

initialConverterData :: ConverterData
initialConverterData =
  ConverterData
    { _fiatAmount = Amount 1.000000,
      _btcAmount = Amount 0.00000010,
      _satsAmount = Amount 100
    }

mkConverterForm :: ConverterData -> ConverterForm
mkConverterForm =
  newForm
    [ editShowableField fiatAmount ConverterFiatField,
      editShowableField btcAmount ConverterBtcField,
      editShowableField satsAmount ConverterSatField
    ]

drawConverter :: TUIState -> Widget TUIResource
drawConverter st =
  hCenter $
    vBox
      [ padBottom (Pad 2) $ hCenter $ withBold $ str "Converter " <+> loadingAnimation,
        padTopBottom 1 $ hCenter $ hLimit 30 $ renderForm (st ^. converterForm),
        str $ show (st ^. stLastBrickEvent),
        str $ show $ formState (st ^. converterForm),
        str $ show $ focusGetCurrent $ formFocus (st ^. converterForm),
        padBottom (Pad 1) $ str $ show (st ^. selectedFiat)
      ]
  where
    loadingAnimation =
      let spinner = drawSpinner (st ^. tick)
       in case st ^. prices of
            NotAsked -> spinner
            Loading _ -> spinner
            _ -> emptyStr

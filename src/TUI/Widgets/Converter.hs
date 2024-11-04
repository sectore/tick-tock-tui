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
import TUI.Service.Types (Amount (..))
import TUI.Types (ConverterData (..), ConverterForm, TUIResource (..), TUIState, btcAmount, converterForm, fiatAmount, satsAmount, selectedFiat, stLastBrickEvent)

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
  vBox
    [ str "Converter",
      str $ "Last event: " <> show (st ^. stLastBrickEvent),
      str $ show $ formState (st ^. converterForm),
      str $ show $ focusGetCurrent $ formFocus (st ^. converterForm),
      str $ show (st ^. selectedFiat),
      padTop (Pad 1) $ hLimit 50 $ hCenter $ renderForm (st ^. converterForm)
    ]

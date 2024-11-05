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
import TUI.Service.Types (Amount (..), RemoteData (Loading, NotAsked))
import TUI.Types (ConverterData (..), ConverterForm, TUIResource (..), TUIState, btcAmount, converterForm, fiatAmount, prices, satsAmount, tick)
import TUI.Utils (emptyStr)
import TUI.Widgets.Loader (drawSpinner)

initialConverterData :: ConverterData
initialConverterData =
  ConverterData
    { _fiatAmount = Amount 5,
      _btcAmount = Amount 0,
      _satsAmount = Amount 0
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
        padTopBottom 1 $ hCenter $ hLimit 30 $ renderForm (st ^. converterForm)
      ]
  where
    loadingAnimation =
      let spinner = drawSpinner (st ^. tick)
       in case st ^. prices of
            NotAsked -> spinner
            Loading _ -> spinner
            _ -> emptyStr

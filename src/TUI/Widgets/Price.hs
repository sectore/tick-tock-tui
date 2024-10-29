module TUI.Widgets.Price (drawPrice) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (Fiat (..), Prices (..), PricesRD, RemoteData (..))
import TUI.Types (TUIState (..), prices, selectedFiat)
import TUI.Utils (emptyStr, loadingStr)

drawPrice :: TUIState -> Widget ()
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
                    [ [ padRight (Pad 10) $ withBold $ str "1 BTC",
                        padLeft (Pad 10) $ rdToStr rdPrices
                      ]
                    ]
    ]
  where
    rdPrices = st ^. prices

    loadingAnimation = case rdPrices of
      Loading _ -> loadingStr
      _ -> emptyStr

    priceStr = case st ^. selectedFiat of
      FiatEUR -> str . show . pEUR
      FiatUSD -> str . show . pUSD
      FiatGBP -> str . show . pGBP
      FiatCAD -> str . show . pCAD
      FiatCHF -> str . show . pCHF
      FiatAUD -> str . show . pAUD
      FiatJPY -> str . show . pJPY

    rdToStr rd = case rd of
      NotAsked -> loadingStr
      Loading mp -> maybe loadingStr priceStr mp
      Failure _ -> withError $ str "error"
      Success p -> priceStr p

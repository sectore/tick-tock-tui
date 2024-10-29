module TUI.Widgets.Price (drawPrice) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (Fiat (..), Prices (..), RemoteData (..))
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
      EUR -> str . show . pEUR
      USD -> str . show . pUSD
      GBP -> str . show . pGBP
      CAD -> str . show . pCAD
      CHF -> str . show . pCHF
      AUD -> str . show . pAUD
      JPY -> str . show . pJPY

    rdToStr rd = case rd of
      NotAsked -> loadingStr
      Loading mp -> maybe loadingStr priceStr mp
      Failure _ -> withError $ str "error"
      Success p -> priceStr p

module TUI.Widgets.Price (drawPrice) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (Prices (..), PricesRD, RemoteData (..))
import TUI.Types (TUIState (..), prices, selectedFiat)
import TUI.Utils (emptyStr, getPriceByFiat, loadingStr)

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
                        padLeft (Pad 10) $ withBold $ rdToStr rdPrices
                      ]
                    ]
    ]
  where
    rdPrices = st ^. prices
    loadingAnimation = case rdPrices of
      Loading _ -> loadingStr
      _ -> emptyStr
    fiat = st ^. selectedFiat
    priceStr :: Prices -> Widget n
    priceStr = str . show . getPriceByFiat fiat
    rdToStr :: forall n. PricesRD -> Widget n
    rdToStr rd = case rd of
      NotAsked -> loadingStr
      Loading ma -> maybe loadingStr priceStr ma
      Failure _ -> withError $ str "error"
      Success a -> priceStr a

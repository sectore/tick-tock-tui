module TUI.Widgets.Price (drawPrice) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (Currency (..), Price (..), Prices (..), PricesRD, RemoteData (..), unPrice)
import TUI.Types (TUIState (..), prices, selectedCurrency)
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
                    [ [ col1Pad $ withBold $ str "1 BTC",
                        col2Pad $ withBold $ rdToStr rdPrices isUSD
                      ],
                      [ col1Pad $ str "",
                        col2Pad $ rdToStr rdPrices (not isUSD)
                      ]
                    ]
    ]
  where
    rdPrices = st ^. prices
    loadingAnimation = case rdPrices of
      Loading _ -> loadingStr
      _ -> emptyStr
    isUSD = (st ^. selectedCurrency) == USD
    col1Pad = padRight (Pad 10)
    col2Pad = padLeft (Pad 10)
    priceStr :: Bool -> Prices -> Widget n
    priceStr renderUSD prices' =
      if renderUSD
        then str "$ " <+> (str . show . unPrice . usd) prices'
        else (str . show . unPrice . eur) prices' <+> str " EUR"
    loadingStr' renderUSD =
      if renderUSD
        then str "$ " <+> loadingStr
        else loadingStr <+> str " EUR"
    rdToStr :: forall n. PricesRD -> Bool -> Widget n
    rdToStr rd renderUSD = case rd of
      NotAsked -> loadingStr' renderUSD
      Loading ma -> maybe (loadingStr' renderUSD) (priceStr renderUSD) ma
      Failure _ -> withError $ str "error"
      Success a -> priceStr renderUSD a

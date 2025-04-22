module TUI.Widgets.Ratio (
  drawRatio,
  mkRatioForm,
  initialRatioData,
)
where

import Brick
import Brick.Forms (
  editShowableFieldWithValidate,
  newForm,
  renderForm,
 )

import Brick.Widgets.Center
import Data.Char (isLetter)
import qualified Data.Text as T
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (
  Fiat (..),
  Price (..),
  Prices (pUSD),
  RemoteData (..),
  Ticker (..),
  unTicker,
 )
import TUI.Types (
  RatioData (..),
  RatioForm,
  TUIResource (..),
  TUIState,
  assetPrice,
  prices,
  ratioForm,
  rdTicker,
  tick,
 )
import TUI.Utils (emptyStr)
import TUI.Widgets.Loader (drawLoadingString4, drawSpinner)
import Text.Printf (printf)

initialRatioData :: Ticker -> RatioData
initialRatioData t =
  RatioData
    { _rdTicker = t
    }

mkRatioForm :: RatioData -> RatioForm
mkRatioForm =
  newForm
    [ tickerField
    ]
  where
    tickerField =
      editShowableFieldWithValidate
        rdTicker
        RatioTickerField
        ( \t ->
            let s = unTicker t
                l = T.length s
             in T.all isLetter s && l > 2 && l < 5 && T.toUpper s /= "BTC"
        )

drawRatio :: TUIState -> Widget TUIResource
drawRatio st =
  vBox
    [ padBottom (Pad 2) $ hCenter $ withBold $ str "RATIO" <+> padLeft (Pad 1) loadingAnimation
    , hCenter $
        hBox
          [ str "BTC/"
          , hLimit 5 $ renderForm (st ^. ratioForm)
          , str "= "
          , rdToRatioStr
          ]
    ]
  where
    rdAssetPrice = st ^. assetPrice
    loadingAnimation =
      let spinner = drawSpinner (st ^. tick)
       in case rdAssetPrice of
            NotAsked -> spinner
            Loading _ -> spinner
            _ -> emptyStr

    rdToRatioStr :: Widget n
    rdToRatioStr =
      let
        -- `ratioStr` -> btc / asset in different precision based on value
        ratioStr :: Price USD -> Price USD -> Widget n
        ratioStr assetPrice' btcPrice = str $ formatRatio (unPrice btcPrice / unPrice assetPrice')

        formatRatio :: Double -> String
        formatRatio ratio
          | ratio < 1 = printf "%.3f" ratio
          | ratio < 100 = printf "%.2f" ratio
          | ratio < 1000 = printf "%.1f" ratio
          | otherwise = printf "%.0f" ratio
       in
        case liftA2 (,) rdAssetPrice (st ^. prices) of
          Success (p, ps) -> ratioStr p (pUSD ps)
          Failure err -> withError $ str err
          _ -> drawLoadingString4 (st ^. tick)

module TUI.Widgets.Ratio (
  drawRatio,
  mkRatioForm,
  initialRatioData,
)
where

import Brick
import Brick.Forms (
  Form (formState),
  editShowableFieldWithValidate,
  newForm,
  renderForm,
 )

import Brick.Widgets.Center
import Brick.Widgets.Table (
  ColumnAlignment (..),
  columnBorders,
  renderTable,
  rowBorders,
  setDefaultColAlignment,
  surroundingBorder,
  table,
 )
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
  editMode,
  extraInfo,
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
    [ padBottom (Pad 2) $ hCenter $ withBold $ str "RATIO" <+> loadingAnimation
    , hCenter $
        renderTable $
          surroundingBorder False $
            rowBorders False $
              columnBorders False $
                setDefaultColAlignment AlignLeft $
                  table
                    [
                      [ col1 $ str "pair"
                      , col2 $
                          -- one more `Pad` if cursor is visible
                          padRight (Pad $ if editable then 12 else 11) $
                            hBox
                              [ str "BTC/"
                              , hLimit 5 $ if editable then renderForm rf else str $ show $ formState rf ^. rdTicker
                              ]
                      ]
                    ,
                      [ col1 $
                          str
                            "ratio"
                      , col2 $ (if st ^. extraInfo then withBold else id) rdToRatioStr
                      ]
                    ,
                      [ emptyWidget
                      , if st ^. extraInfo then col2 rdToPricesStr else emptyWidget
                      ]
                    ]
    ]
  where
    rf = st ^. ratioForm
    editable = st ^. editMode
    rdAssetPrice = st ^. assetPrice
    loadingAnimation =
      let spinner = padLeft (Pad 1) $ drawSpinner (st ^. tick)
       in case rdAssetPrice of
            NotAsked -> spinner
            Loading _ -> spinner
            _ -> emptyStr
    col1 = withBold . padRight (Pad 1) . padLeft (Pad 8)
    col2 = padLeft (Pad 10) . padRight (Pad 1)
    rdToRatioStr :: Widget n
    rdToRatioStr =
      let
        ratioStr :: Price USD -> Price USD -> Widget n
        ratioStr assetPrice' btcPrice = str $ formatRatio (unPrice btcPrice / unPrice assetPrice')

        -- Use different precision based on value
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

    rdToPricesStr :: Widget n
    rdToPricesStr =
      let
        pricesStr :: Price USD -> Price USD -> Widget n
        pricesStr assetPrice' btcPrice = str $ show btcPrice ++ "/" ++ show assetPrice'
       in
        case liftA2 (,) rdAssetPrice (st ^. prices) of
          Success (p, ps) -> pricesStr p (pUSD ps)
          Failure err -> withError $ str err
          _ -> drawLoadingString4 (st ^. tick)

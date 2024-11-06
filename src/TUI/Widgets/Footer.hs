module TUI.Widgets.Footer where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Core
  ( Padding (..),
    fill,
    hBox,
    hLimit,
    hLimitPercent,
    padLeft,
    padRight,
    str,
    vBox,
    vLimit,
    withBorderStyle,
    (<+>),
  )
import Brick.Widgets.ProgressBar qualified as P
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (Fiat (..), Prices (..), RemoteData (..))
import TUI.Types (TUIResource (..), TUIState, View (..), animate, currentView, fetchTick, lastFetchTick, prices, selectedFiat, showMenu)
import TUI.Utils (fps, loadingStr, maxFetchTick)
import Text.Printf (printf)

drawFooter :: TUIState -> Widget TUIResource
drawFooter st =
  hLimitPercent 100 $
    vBox $
      [ hBox
          [ str "1 BTC = " <+> rdToStr rdPrices,
            vLimit 1 $ fill ' ',
            str "next [r]eload " <+> progress <+> tickTime
          ],
        withBorderStyle BS.ascii $ B.hBorderWithLabel $ str $ "[m]enu " <> if st ^. showMenu then "↓" else "↑"
      ]
        <> ( [ renderTable $
                 surroundingBorder False $
                   rowBorders False $
                     columnBorders False $
                       setDefaultColAlignment AlignLeft $
                         table
                           [ [col1 $ str "screens", views],
                             [col1 $ str "actions", actions]
                           ]
               | st ^. showMenu
             ]
           )
  where
    rdPrices = st ^. prices
    sFiat = st ^. selectedFiat
    priceStr = case sFiat of
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

    col1 = padRight (Pad 6) . withBold
    foldWithSpace = foldl1 (\x y -> x <+> (padLeft $ Pad 3) y)
    viewLabels =
      [ (FeesView, "[f]ees"),
        (BlockView, "[b]lock"),
        (ConverterView, "[c]onverter")
      ]
    v = st ^. currentView
    views =
      foldWithSpace
        -- create a list of view labels
        [ if v' == v
            then -- bold label for `currentView`
              withBold (str label)
            else str label
          | (v', label) <- viewLabels
        ]
    animationLabel = "[a]nimation " ++ if st ^. animate then "stop" else "start"
    actionLabels = case v of
      FeesView -> ["[r]eload data", animationLabel]
      BlockView -> ["[r]eload data", animationLabel]
      ConverterView -> ["[r]eload data", "[t]oggle btc|sats", "[s]witch fiat", animationLabel]
    actions = foldWithSpace $ str <$> actionLabels
    remainingTick = maxFetchTick - (st ^. fetchTick - st ^. lastFetchTick)
    percent :: Float
    -- 1.1 => tweaked by 0.1 to have a completed progressbar visible just before 100%
    percent = 1.1 - fromIntegral remainingTick / fromIntegral maxFetchTick
    progress =
      hLimit 10 $
        padRight (Pad 2) $
          P.customProgressBar
            '─'
            '─'
            Nothing
            percent
    tickTime =
      let total = remainingTick `div` fps
          mm = total `div` 60
          ss = total `mod` 60
       in str $ printf "%02dm %02ds" mm ss

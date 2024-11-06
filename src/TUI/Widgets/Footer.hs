module TUI.Widgets.Footer where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( Padding (..),
    hLimit,
    padLeft,
    padRight,
    str,
    (<+>),
  )
import Brick.Widgets.ProgressBar qualified as P
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold)
import TUI.Types (TUIResource (..), TUIState, View (..), animate, currentView, fetchTick, lastFetchTick)
import TUI.Utils (fps, maxFetchTick)
import Text.Printf (printf)

drawFooter :: TUIState -> Widget TUIResource
drawFooter st =
  renderTable $
    surroundingBorder False $
      rowBorders False $
        columnBorders False $
          setDefaultColAlignment AlignLeft $
            table
              [ [col1 $ str "Screens", views],
                [col1 $ str "Actions", actions],
                [col1 $ str "Auto reload", progress <+> tickTime]
              ]
  where
    v = st ^. currentView
    col1 = padRight (Pad 6) . withBold
    foldWithSpace = foldl1 (\x y -> x <+> (padLeft $ Pad 3) y)
    viewLabels =
      [ (PriceView, "[p]rice"),
        (FeesView, "[f]ees"),
        (BlockView, "[b]lock"),
        (ConverterView, "[c]onverter")
      ]
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
      FeesView -> ["[r]eload data", "[t]oggle value", animationLabel]
      PriceView -> ["[r]eload data", "[t]oggle btc|sats", "[s]witch fiat", animationLabel]
      BlockView -> ["[r]eload data"]
      ConverterView -> ["[r]eload data", "[t]oggle btc|sats", "[s]witch fiat", animationLabel]
    actions = foldWithSpace $ str <$> actionLabels
    remainingTick = maxFetchTick - (st ^. fetchTick - st ^. lastFetchTick)
    percent :: Float
    -- 1.1 => tweaked by 0.1 to have a completed progressbar visible just before 100%
    percent = 1.1 - fromIntegral remainingTick / fromIntegral maxFetchTick
    progress =
      hLimit 15 $
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

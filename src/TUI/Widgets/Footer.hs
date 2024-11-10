{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move brackets to avoid $" #-}
module TUI.Widgets.Footer where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Core
  ( Padding (..),
    emptyWidget,
    padLeft,
    padRight,
    str,
    vBox,
    withBorderStyle,
    (<+>),
  )
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold)
import TUI.Service.Types (Bitcoin (BTC))
import TUI.Types (TUIResource (..), TUIState, View (..), animate, currentView, selectedBitcoin, showMenu)
import TUI.Widgets.Countdown (drawCountdown)

drawFooter :: TUIState -> Widget TUIResource
drawFooter st =
  vBox $
    [ padLeft (Pad 1) $ drawCountdown st,
      withBorderStyle BS.ascii $ B.hBorderWithLabel $ str $ "[m]enu " <> if st ^. showMenu then "↓" else "↑"
    ]
      <> ( [ renderTable $
               surroundingBorder False $
                 rowBorders False $
                   columnBorders False $
                     setDefaultColAlignment AlignLeft $
                       table
                         [ [col1 $ str "screens", views],
                           [col1 $ str "actions", actions],
                           [col1 emptyWidget, actions2]
                         ]
             | st ^. showMenu
           ]
         )
  where
    col1 = padLeft (Pad 1) . padRight (Pad 6) . withBold
    foldWithSpace = foldl1 (\x y -> x <+> (padLeft $ Pad 2) y)
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
    actionLabels =
      [ "[r]eload data",
        "[s]witch to " ++ if st ^. selectedBitcoin == BTC then "sat" else "btc",
        "[t]oggle fiat"
      ]
    actionLabels2 =
      [ "[e]xtra info",
        "[a]nimation " ++ if st ^. animate then "stop" else "start",
        "[q]uit"
      ]
    actions = foldWithSpace $ str <$> actionLabels
    actions2 = foldWithSpace $ str <$> actionLabels2

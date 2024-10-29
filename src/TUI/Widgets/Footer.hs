module TUI.Widgets.Footer where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( Padding (..),
    padLeft,
    padRight,
    str,
    vBox,
    (<+>),
  )
import TUI.Attr (withBold)
import TUI.Types

drawFooter :: View -> Widget ()
drawFooter v =
  vBox
    [ headline "Actions" <+> actions,
      headline "Screens" <+> views
    ]
  where
    headline = padRight (Pad 4) . withBold . str
    foldWithSpace = foldl1 (\x y -> x <+> (padLeft $ Pad 4) y)
    viewLabels =
      [ (FeesView, "[1] Fees"),
        (PriceView, "[2] Price"),
        (BlockView, "[3] Block"),
        (ConverterView, "[4] Converter")
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
    actionLabels = case v of
      FeesView -> ["[r] Reload fees", "[t] Toggle value", "[a] Toggle animation"]
      PriceView -> ["[r] Reload price", "[t] Toggle BTC|sats", "[s] Switch fiat"]
      BlockView -> ["[r] Reload block data", "[a] Toggle animation"]
      ConverterView -> ["[r] Reload price", "[t] Toggle USD|EUR", "[a] Toggle animation"]
      DraftView -> [""]
    actions = foldWithSpace $ str <$> actionLabels

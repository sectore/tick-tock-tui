module TUI.Widgets.Ratio (
  drawRatio,
)
where

import Brick

import Brick.Widgets.Center
import TUI.Attr (withBold)
import TUI.Types (TUIResource, TUIState)

drawRatio :: TUIState -> Widget TUIResource
drawRatio _ =
  vBox
    [ padBottom (Pad 2) $ hCenter $ withBold $ str "Ratio"
    ]

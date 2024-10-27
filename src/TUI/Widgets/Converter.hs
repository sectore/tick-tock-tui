module TUI.Widgets.Converter (drawConverter) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( str,
    vBox,
  )
import Lens.Micro ((^.))
import TUI.Types (TUIState, price)

drawConverter :: TUIState -> Widget ()
drawConverter st =
  vBox
    [ str "Converter",
      str . show $ st ^. price
    ]

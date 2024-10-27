module TUI.Widgets.Fees (drawFees) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( str,
    vBox,
  )
import Lens.Micro ((^.))
import TUI.Types (TUIState, fees)

drawFees :: TUIState -> Widget ()
drawFees st =
  vBox
    [ str "Fees",
      str . show $ st ^. fees
    ]

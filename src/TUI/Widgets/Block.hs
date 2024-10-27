module TUI.Widgets.Block (drawBlock) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( str,
    vBox,
  )
import Lens.Micro ((^.))
import TUI.Types (TUIState, prices)

drawBlock :: TUIState -> Widget ()
drawBlock st =
  vBox
    [ str "Block",
      str . show $ st ^. prices
    ]

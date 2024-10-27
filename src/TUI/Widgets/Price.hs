module TUI.Widgets.Price (drawPrice) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( str,
    vBox,
  )
import Lens.Micro ((^.))
import TUI.Types (TUIState, price)

drawPrice :: TUIState -> Widget ()
drawPrice st =
  vBox
    [ str "Price",
      str . show $ st ^. price
    ]

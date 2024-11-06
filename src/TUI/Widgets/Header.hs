module TUI.Widgets.Header (drawHeader) where

import Brick ((<+>))
import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( padTopBottom,
    str,
  )
import Lens.Micro ((^.))
import TUI.Attr (withBtcColor)
import TUI.Types (TUIResource (..), TUIState, animate, tick)

drawHeader :: TUIState -> Widget TUIResource
drawHeader st =
  padTopBottom 1 $ txt
  where
    t = st ^. tick `div` 30
    i = mod t 5
    fullText = str "TICK TOCK NEXT " <+> withBtcColor (str "B") <+> str "LOCK"
    txt =
      if st ^. animate
        then case i of
          0 -> str "TICK"
          1 -> str "TICK TOCK"
          2 -> str "TICK TOCK NEXT"
          3 -> fullText
          _ -> str " "
        else fullText

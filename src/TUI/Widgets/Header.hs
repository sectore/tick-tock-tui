module TUI.Widgets.Header (drawHeader) where

import Brick ((<+>))
import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( fill,
    hBox,
    padLeftRight,
    str,
    vLimit,
  )
import Lens.Micro ((^.))
import TUI.Attr (withBtcColor)
import TUI.Types (TUIResource (..), TUIState, animate, tick)
import TUI.Widgets.Price

drawHeader :: TUIState -> Widget TUIResource
drawHeader st =
  padLeftRight 1 $ hBox [txt, vLimit 1 $ fill ' ', drawPrice st]
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

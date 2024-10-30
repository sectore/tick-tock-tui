module TUI.Widgets.Header (drawHeader) where

import Brick ((<+>))
import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center
import Brick.Widgets.Core
  ( Padding (..),
    padBottom,
    padTop,
    str,
  )
import Lens.Micro ((^.))
import TUI.Attr (withBtcColor)
import TUI.Types (TUIState, animate, tick)

drawHeader :: TUIState -> Widget ()
drawHeader st =
  hCenter $ padTop (Pad 1) $ padBottom (Pad 1) txt
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

module TUI.Attr where

import Brick.AttrMap qualified as A
import Brick.Types
  ( Widget,
  )
import Brick.Util (fg)
import Brick.Widgets.Core
  ( withAttr,
  )
import Brick.Widgets.ProgressBar qualified as P
import Graphics.Vty qualified as V

boldAttr :: A.AttrName
boldAttr = A.attrName "bold"

errorAttr :: A.AttrName
errorAttr = A.attrName "error"

withBold :: forall n. Widget n -> Widget n
withBold = withAttr boldAttr

withError :: forall n. Widget n -> Widget n
withError = withAttr errorAttr

-- Bitcoin color: F7931A or RGB(247, 147, 26)
-- @see Bitcoin Design: https://bitcoin.design/guide/getting-started/visual-language
btcColor :: V.Color
btcColor = V.RGBColor 247 147 26

tuiAttrMap :: A.AttrMap
tuiAttrMap =
  A.attrMap
    V.defAttr
    [ (boldAttr, V.defAttr `V.withStyle` V.bold),
      (errorAttr, fg V.red),
      (P.progressCompleteAttr, fg btcColor)
    ]

module TUI.Attr where

import qualified Brick.AttrMap as A
import qualified Brick.Forms as F
import Brick.Types (
  Widget,
 )
import Brick.Util (bg, fg)
import Brick.Widgets.Core (
  withAttr,
 )
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V

boldAttr :: A.AttrName
boldAttr = A.attrName "bold"

errorAttr :: A.AttrName
errorAttr = A.attrName "error"

btcColorAttr :: A.AttrName
btcColorAttr = A.attrName "btcColor"

withBold :: forall n. Widget n -> Widget n
withBold = withAttr boldAttr

withError :: forall n. Widget n -> Widget n
withError = withAttr errorAttr

withBtcColor :: forall n. Widget n -> Widget n
withBtcColor = withAttr btcColorAttr

-- Bitcoin color: F7931A or RGB(247, 147, 26)
-- @see Bitcoin Design: https://bitcoin.design/guide/getting-started/visual-language
btcColor' :: V.Color
btcColor' = V.RGBColor 247 147 26

tuiAttrMap :: A.AttrMap
tuiAttrMap =
  A.attrMap
    V.defAttr
    [ (boldAttr, V.defAttr `V.withStyle` V.bold)
    , (errorAttr, fg V.red)
    , (btcColorAttr, fg btcColor')
    , (P.progressCompleteAttr, fg btcColor')
    , (F.invalidFormInputAttr, bg V.red)
    ]

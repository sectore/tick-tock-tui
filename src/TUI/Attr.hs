module TUI.Attr where

import Brick.AttrMap qualified as A
import Graphics.Vty qualified as V

boldAttr :: A.AttrName
boldAttr = A.attrName "bold"

tuiAttrMap :: A.AttrMap
tuiAttrMap =
  A.attrMap
    V.defAttr
    [ (boldAttr, V.defAttr `V.withStyle` V.bold)
    ]

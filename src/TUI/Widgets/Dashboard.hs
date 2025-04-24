module TUI.Widgets.Dashboard (
  drawDashboard,
)
where

import Brick

import Brick.Widgets.Center

import Brick.Widgets.Border.Style as BS
import TUI.Types (
  TUIResource,
  TUIState,
 )
import TUI.Widgets.Block (drawBlock)
import TUI.Widgets.Converter (drawConverter)
import TUI.Widgets.Fees (drawFees)
import TUI.Widgets.Ratio (drawRatio)

drawDashboard :: TUIState -> Widget TUIResource
drawDashboard st =
  vBox
    [ hBorder'
    , hBox
        [ hLimitPercent 50 $ center $ vCenter $ drawFees st
        , vBorder'
        , hLimitPercent 100 $ center $ vCenter $ drawBlock st
        ]
    , hBorder'
    , hBox
        [ hLimitPercent 50 $ center $ vCenter $ drawConverter st
        , vBorder'
        , hLimitPercent 100 $ center $ vCenter $ drawRatio st
        ]
    ]
  where
    hBorder' = vLimit 1 $ fill $ BS.bsHorizontal BS.defaultBorderStyle
    vBorder' = hLimit 1 $ fill $ BS.bsVertical BS.defaultBorderStyle

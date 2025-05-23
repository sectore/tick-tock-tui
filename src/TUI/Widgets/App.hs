module TUI.Widgets.App where

import Brick.Types (
  Widget,
 )
import Brick.Widgets.Center
import Brick.Widgets.Core
import Lens.Micro ((^.))
import TUI.Config (Config)
import TUI.Types
import TUI.Widgets.Block (drawBlock)
import TUI.Widgets.Converter (drawConverter)
import TUI.Widgets.Dashboard (drawDashboard)
import TUI.Widgets.Fees (drawFees)
import TUI.Widgets.Footer (drawFooter)
import TUI.Widgets.Header (drawHeader)
import TUI.Widgets.Ratio (drawRatio)

drawApp :: Config -> TUIState -> [Widget TUIResource]
drawApp conf st = [ui]
  where
    cv = st ^. currentView
    main = case cv of
      FeesView -> drawFees st
      BlockView -> drawBlock st
      ConverterView -> drawConverter st
      RatioView -> drawRatio st
      DashboardView -> drawDashboard st
    ui =
      vBox
        [ drawHeader st
        , hCenter $ vCenter main
        , drawFooter st conf
        ]

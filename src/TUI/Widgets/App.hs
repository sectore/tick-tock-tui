module TUI.Widgets.App where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center
import Brick.Widgets.Core
import Lens.Micro ((^.))
import TUI.Config (Config, cfgMempoolUrl)
import TUI.Types
import TUI.Widgets.Block (drawBlock)
import TUI.Widgets.Converter (drawConverter)
import TUI.Widgets.Fees (drawFees)
import TUI.Widgets.Footer (drawFooter)
import TUI.Widgets.Header (drawHeader)

drawApp :: Config -> TUIState -> [Widget TUIResource]
drawApp conf st = [padTopBottom 1 ui]
  where
    cv = st ^. currentView
    main = case cv of
      FeesView -> drawFees st
      BlockView -> drawBlock st
      ConverterView -> drawConverter st
    ui =
      vBox
        [ drawHeader st,
          hCenter $ vCenter main,
          drawFooter st (cfgMempoolUrl conf)
        ]

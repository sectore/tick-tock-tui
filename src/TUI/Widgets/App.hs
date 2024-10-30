module TUI.Widgets.App where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center
import Brick.Widgets.Core
import Lens.Micro ((^.))
import TUI.Types
import TUI.Widgets.Block (drawBlock)
import TUI.Widgets.Converter (drawConverter)
import TUI.Widgets.Draft (drawDraft)
import TUI.Widgets.Fees (drawFees)
import TUI.Widgets.Footer (drawFooter)
import TUI.Widgets.Price (drawPrice)

drawApp :: TUIState -> [Widget ()]
drawApp st = [ui]
  where
    cv = st ^. currentView
    main = case cv of
      FeesView -> drawFees st
      PriceView -> drawPrice st
      BlockView -> drawBlock st
      ConverterView -> drawConverter st
      DraftView -> drawDraft st
    ui =
      vBox
        [ str "header",
          hCenter $ vCenter main,
          drawFooter st
        ]

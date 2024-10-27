module TUI.Widgets.Fees (drawFees) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( Padding (..),
    padBottom,
    padLeft,
    padRight,
    str,
    vBox,
    (<+>),
  )
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (Fees (..), FeesRD, RemoteData (..))
import TUI.Types (TUIState (..), fees)
import TUI.Utils (emptyStr, loadingStr)

drawFees :: TUIState -> Widget ()
drawFees st =
  vBox
    [ hCenter $ padBottom (Pad 2) $ withBold $ str "Fees " <+> loadingAnimation,
      hCenter $
        renderTable $
          surroundingBorder False $
            rowBorders False $
              columnBorders False $
                setDefaultColAlignment AlignLeft $
                  table
                    [ [ col1Left (str "fast") <+> col1Right (str "~10min"),
                        col2Left (rdToStr fast (st ^. fees)) <+> col1Right (str "sat/vB")
                      ],
                      [ col1Left (str "medium") <+> col1Right (str "~30min"),
                        col2Left (rdToStr medium (st ^. fees)) <+> col1Right (str "sat/vB")
                      ],
                      [ col1Left (str "slow") <+> col1Right (str "~60min"),
                        col2Left (rdToStr slow (st ^. fees)) <+> col1Right (str "sat/vB")
                      ]
                    ]
    ]
  where
    rdFees = st ^. fees
    loadingAnimation = case rdFees of
      Loading _ -> loadingStr
      _ -> emptyStr
    col1Left = withBold . padRight (Pad 1)
    col1Right = padRight (Pad 10)
    col2Left = padLeft (Pad 10) . padRight (Pad 1)
    rdToStr :: forall a n. (Show a) => (Fees -> a) -> FeesRD -> Widget n
    rdToStr l rd = case rd of
      NotAsked -> loadingStr
      Loading ma -> maybe loadingStr (str . show . l) ma
      Failure _ -> withError $ str "error"
      Success a -> withBold . str $ show $ l a

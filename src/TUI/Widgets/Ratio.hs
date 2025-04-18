module TUI.Widgets.Ratio (
  drawRatio,
)
where

import Brick

import Brick.Widgets.Center
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (RemoteData (..))
import TUI.Types (TUIResource, TUIState, assetPrice, tick)
import TUI.Utils (emptyStr)
import TUI.Widgets.Loader (drawLoadingString4, drawSpinner)

drawRatio :: TUIState -> Widget TUIResource
drawRatio st =
  vBox
    [ padBottom (Pad 2) $ hCenter $ withBold $ str "Ratio" <+> padLeft (Pad 1) loadingAnimation
    , hCenter rdToStr
    ]
  where
    rdAssetPrice = st ^. assetPrice
    loadingAnimation =
      let spinner = drawSpinner (st ^. tick)
       in case rdAssetPrice of
            NotAsked -> spinner
            Loading _ -> spinner
            _ -> emptyStr
    rdToStr :: Widget n
    rdToStr =
      let loadingStr = drawLoadingString4 (st ^. tick)
       in case rdAssetPrice of
            NotAsked -> loadingStr
            Loading Nothing -> loadingStr
            Loading (Just p) -> str $ show p
            Failure _ -> withError $ str "error"
            Success p -> str $ show p

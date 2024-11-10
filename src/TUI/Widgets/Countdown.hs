module TUI.Widgets.Countdown where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( emptyWidget,
    hLimit,
    str,
    (<+>),
    (<=>),
  )
import Brick.Widgets.ProgressBar qualified as P
import Lens.Micro ((^.))
import TUI.Types (TUIResource (..), TUIState, extraInfo, fetchTick, lastFetchTick)
import TUI.Utils (fps, maxFetchTick)
import Text.Printf (printf)

drawCountdown :: TUIState -> Widget TUIResource
drawCountdown st =
  (if st ^. extraInfo then str "auto reload in " <+> tickTime else emptyWidget) <=> progress
  where
    remainingTick = maxFetchTick - (st ^. fetchTick - st ^. lastFetchTick)
    percent :: Float
    -- 1.1 => tweaked by 0.1 to have a completed progressbar visible just before 100%
    percent = 1.1 - fromIntegral remainingTick / fromIntegral maxFetchTick
    progress =
      hLimit 22 $
        P.customProgressBar
          '─'
          '─'
          Nothing
          percent
    tickTime =
      let total = remainingTick `div` fps
          mm = total `div` 60
          ss = total `mod` 60
       in str $ printf "%02dm %02ds" mm ss

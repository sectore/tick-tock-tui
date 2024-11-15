module TUI.Widgets.Countdown where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( hLimit,
    padLeftRight,
    str,
    (<+>),
  )
import Brick.Widgets.ProgressBar qualified as P
import Lens.Micro ((^.))
import TUI.Types (TUIResource (..), TUIState, fetchTick, lastFetchTick, maxFetchTick)
import TUI.Utils (fps)
import Text.Printf (printf)

drawCountdown :: TUIState -> Widget TUIResource
drawCountdown st =
  progress
    <+> tickTime
  where
    maxTick = st ^. maxFetchTick
    remainingTick = maxTick - (st ^. fetchTick - st ^. lastFetchTick)
    percent :: Float
    -- 1.1 => tweaked by 0.1 to have a completed progressbar visible just before 100%
    percent = 1.1 - fromIntegral remainingTick / fromIntegral maxTick
    progress =
      hLimit 14 $
        padLeftRight 1 $
          P.customProgressBar
            '─'
            '─'
            Nothing
            percent
    tickTime =
      let total = remainingTick `div` fps
          mm = total `div` 60
          ss = total `mod` 60
       in str $ printf "%02d:%02d" mm ss

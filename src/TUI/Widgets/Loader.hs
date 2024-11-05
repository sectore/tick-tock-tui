module TUI.Widgets.Loader (drawSpinner) where

import Brick (str)
import Brick.Types
  ( Widget,
  )
import TUI.Types (TUIResource (..))

drawSpinner :: Int -> Widget TUIResource
drawSpinner = drawLoader chars 5
  where
    chars =
      [ '⠋',
        '⠙',
        '⠹',
        '⠸',
        '⠼',
        '⠴',
        '⠦',
        '⠧',
        '⠇',
        '⠏'
      ]

drawLoader :: [Char] -> Int -> Int -> Widget TUIResource
drawLoader chars speed tick =
  str [chars !! i]
  where
    t = tick `div` speed
    i :: Int = mod t $ length chars

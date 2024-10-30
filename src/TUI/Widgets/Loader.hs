module TUI.Widgets.Loader (drawSpinner) where

import Brick (str)
import Brick.Types
  ( Widget,
  )

drawSpinner :: Int -> Widget ()
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

drawLoader :: [Char] -> Int -> Int -> Widget ()
drawLoader chars speed tick =
  str [chars !! i]
  where
    t = tick `div` speed
    i :: Int = mod t $ length chars

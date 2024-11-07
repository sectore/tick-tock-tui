module TUI.Widgets.Loader
  ( drawSpinner,
    drawLoadingString2,
    drawLoadingString3,
    drawLoadingString4,
  )
where

import Brick (str)
import Brick.Types
  ( Widget,
  )

drawSpinner :: forall n. Int -> Widget n
drawSpinner = drawLoader chars 5
  where
    chars =
      [ "⠋",
        "⠙",
        "⠹",
        "⠸",
        "⠼",
        "⠴",
        "⠦",
        "⠧",
        "⠇",
        "⠏"
      ]

drawLoadingString4 :: forall n. Int -> Widget n
drawLoadingString4 = drawLoader chars 8
  where
    chars =
      [ ".   ",
        "..  ",
        "... ",
        "....",
        " ...",
        "  ..",
        "   .",
        "  ..",
        " ...",
        "....",
        "... ",
        "..  "
      ]

drawLoadingString3 :: forall n. Int -> Widget n
drawLoadingString3 = drawLoader chars 8
  where
    chars =
      [ ".  ",
        ".. ",
        "...",
        " ..",
        "  .",
        " ..",
        "...",
        ".. "
      ]

drawLoadingString2 :: forall n. Int -> Widget n
drawLoadingString2 = drawLoader chars 8
  where
    chars =
      [ "⠄",
        "⠤",
        "⠠",
        "⠤"
      ]

drawLoader :: forall n. [String] -> Int -> Int -> Widget n
drawLoader chars speed tick =
  str (chars !! i)
  where
    t = tick `div` speed
    i :: Int = mod t $ length chars

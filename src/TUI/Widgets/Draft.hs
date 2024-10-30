module TUI.Widgets.Draft (drawDraft) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Core
  ( str,
    vBox,
    (<=>),
  )
import Data.Text (pack, toUpper, unpack)
import Lens.Micro ((^.))
import TUI.Types (TUIState)

-- eighthBlocks :: (Eq a, Num a, Ord a) => a -> Char
-- eighthBlocks n
--   | n <= 0 = ' '
--   | n == 1 = '▁'
--   | n == 2 = '▂'
--   | n == 3 = '▃'
--   | n == 4 = '▄'
--   | n == 5 = '▅'
--   | n == 6 = '▆'
--   | n == 7 = '▇'
--   | otherwise = '█'

blocks :: [Char]
blocks = [' ', '▁', '▂', '▄', '▅', '▆', '▇', '█', '▇', '▆', '▆', '▄', '▂', '▁']

spinner :: [Char]
spinner =
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

bar :: [String]
bar =
  [ "▒▒▒▒▒▒▒▒▒▒",
    "█▒▒▒▒▒▒▒▒▒",
    "███▒▒▒▒▒▒▒",
    "█████▒▒▒▒▒",
    "███████▒▒▒",
    "██████████",
    "▒▒▒███████",
    "▒▒▒▒▒█████",
    "▒▒▒▒▒▒▒███",
    "▒▒▒▒▒▒▒▒▒█",
    "▒▒▒▒▒▒▒▒▒▒",
    "▒▒▒▒▒▒▒▒▒█",
    "▒▒▒▒▒▒▒███",
    "▒▒▒▒▒█████",
    "▒▒▒███████",
    "██████████",
    "███████▒▒▒",
    "█████▒▒▒▒▒",
    "███▒▒▒▒▒▒▒",
    "█▒▒▒▒▒▒▒▒▒"
  ]

slogan :: [String]
slogan = ["Tick", "Tock", "Next", "Block"]

clock :: [Char]
clock = ['🕐', '🕑', '🕒', '🕓', '🕔', '🕕', '🕖', '🕗', '🕘', '🕙', '🕚', '🕛']

drawDraft :: TUIState -> Widget ()
drawDraft _ =
  vBox
    [ str "Draft"
    -- a
    ]

-- where
--   t = st ^. tick `div` 5
--   tsl = st ^. tick `div` 40
--   sl = mod tsl $ length slogan
--   b :: Int = mod t $ length blocks
--   bb :: Int = mod t $ length bar
--   s :: Int = mod t $ length spinner
--   c = mod tsl $ length clock
--   a =
--     str ("tick: " <> show (st ^. tick))
--       <=> str ("" <> show b)
--       <=> str ("" <> [spinner !! s])
--       <=> str ("" <> [blocks !! b])
--       <=> str (bar !! bb)
--       <=> str ([clock !! c] <> " " <> (unpack . toUpper . pack $ slogan !! sl))

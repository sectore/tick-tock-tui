module TUI.Widgets.Block (drawBlock) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( Padding (..),
    padBottom,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    (<+>),
  )
import Brick.Widgets.Table
import Data.Text (unpack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (utcToLocalTime)
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (Block (..), BlockRD, RemoteData (..))
import TUI.Types (TUIState (..), block, timeZone)
import TUI.Utils (emptyStr, loadingStr, toBtc)
import Text.Printf (printf)

drawBlock :: TUIState -> Widget ()
drawBlock st =
  vBox
    [ hCenter $ padBottom (Pad 2) $ withBold $ str "Latest block " <+> loadingAnimation,
      -- str $ show rdBlock,
      hCenter $
        renderTable $
          surroundingBorder False $
            rowBorders False $
              columnBorders False $
                setDefaultColAlignment AlignLeft $
                  table
                    [ -- block data
                      [ col1Left (str "Height"),
                        col2Left (rdToStr height show rdBlock)
                      ],
                      [ col1Left (str "Timestamp"),
                        col2Left (rdToStr time formatLocalTime rdBlock)
                      ],
                      [ col1Left (str "Size"),
                        col2Left (rdToStr size formatSize rdBlock)
                      ],
                      [ col1Left (str "Txs"),
                        col2Left (rdToStr txs show rdBlock)
                      ],
                      -- miner data
                      [ padTop (Pad 2) $ col1Left (str "Miner"),
                        padTop (Pad 2) $ col2Left (rdToStr poolName unpack rdBlock)
                      ],
                      [ col1Left (str "Total fees"),
                        col2Left (rdToStr poolFees (show . toBtc) rdBlock)
                      ],
                      [ col1Left (str "Reward"),
                        col2Left (rdToStr reward (show . toBtc) rdBlock)
                      ]
                    ]
    ]
  where
    rdBlock = st ^. block
    loadingAnimation = case rdBlock of
      NotAsked -> loadingStr
      Loading _ -> loadingStr
      _ -> emptyStr
    col1Left = padRight (Pad 1)
    col2Left = padLeft (Pad 10) . padRight (Pad 1)
    formatLocalTime = formatTime defaultTimeLocale "%H:%M:%S" . utcToLocalTime (st ^. timeZone)
    -- Format block `size` using decimal (SI) system (similar to Mempool.com)
    -- Others (e.g. Blockstream.com) might use binary (1024-based) based formats
    formatSize bytes
      | bytes >= 1000_000 = printf "%.2f MB" (fromIntegral bytes / 1000_000 :: Double)
      | bytes >= 1000 = show (bytes `div` 1000) ++ " KB"
      | otherwise = show bytes ++ " B"
    rdToStr :: forall a n. (Show a) => (Block -> a) -> (a -> String) -> BlockRD -> Widget n
    rdToStr l show' rd = case rd of
      NotAsked -> loadingStr
      Loading ma -> maybe loadingStr (str . show' . l) ma
      Failure _ -> withError $ str "error"
      Success a -> withBold . str $ show' $ l a

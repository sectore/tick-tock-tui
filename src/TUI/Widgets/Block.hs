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
import TUI.Types (TUIState (..), block, tick, timeZone)
import TUI.Utils (emptyStr, loadingStr, toBtc)
import TUI.Widgets.Loader (drawSpinner)
import Text.Printf (printf)

drawBlock :: TUIState -> Widget ()
drawBlock st =
  vBox
    [ hCenter $ padBottom (Pad 2) $ withBold $ str "Latest block " <+> loadingAnimation,
      hCenter $
        renderTable $
          surroundingBorder False $
            rowBorders False $
              columnBorders False $
                setDefaultColAlignment AlignLeft $
                  table
                    [ -- block data
                      [ col1 (str "Height"),
                        col2 (rdToStr height show rdBlock)
                      ],
                      [ col1 (str "Timestamp"),
                        col2 (rdToStr time formatLocalTime rdBlock)
                      ],
                      [ col1 (str "Size"),
                        col2 (rdToStr size formatSize rdBlock)
                      ],
                      [ col1 (str "Txs"),
                        col2 (rdToStr txs show rdBlock)
                      ],
                      -- miner data
                      [ padTop (Pad 2) $ col1 (str "Miner"),
                        padTop (Pad 2) $ col2 (rdToStr poolName unpack rdBlock)
                      ],
                      [ col1 (str "Fees"),
                        col2 (rdToStr poolFees (show . toBtc) rdBlock)
                      ],
                      [ col1 (str "Reward"),
                        col2 (rdToStr reward (show . toBtc) rdBlock)
                      ]
                    ]
    ]
  where
    rdBlock = st ^. block
    loadingAnimation =
      let spinner = drawSpinner (st ^. tick)
       in case rdBlock of
            NotAsked -> spinner
            Loading _ -> spinner
            _ -> emptyStr
    col1 = withBold . padRight (Pad 1)
    col2 = padLeft (Pad 10) . padRight (Pad 1)
    formatLocalTime = formatTime defaultTimeLocale "%d-%m-%y %H:%M:%S" . utcToLocalTime (st ^. timeZone)
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
      Success a -> str $ show' $ l a

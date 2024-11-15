{-# LANGUAGE DataKinds #-}

module TUI.Widgets.Block (drawBlock) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( Padding (..),
    emptyWidget,
    padBottom,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    (<+>),
  )
import Brick.Widgets.Table
import Data.Text qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (utcToLocalTime)
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (Amount, Bitcoin (..), Block (..), Fiat (..), Prices (..), RemoteData (..))
import TUI.Types (TUIResource (..), TUIState (..), block, extraInfo, prices, selectedFiat, tick, timeZone)
import TUI.Utils (emptyStr, satsToFiat, toBtc)
import TUI.Widgets.Loader (drawLoadingString4, drawSpinner)
import Text.Printf (printf)

drawBlock :: TUIState -> Widget TUIResource
drawBlock st =
  vBox
    [ hCenter $ padBottom (Pad 2) $ withBold $ str "LATEST BLOCK" <+> padLeft (Pad 1) loadingAnimation,
      hCenter $
        renderTable $
          surroundingBorder False $
            rowBorders False $
              columnBorders False $
                setDefaultColAlignment AlignLeft $
                  table
                    [ -- block data
                      [ col1 (str "height"),
                        col2 (rdToStr height show)
                      ],
                      [ col1 (str "timestamp"),
                        col2 (rdToStr time formatLocalTime)
                      ],
                      [ col1 (str "size"),
                        col2 (rdToStr size formatSize)
                      ],
                      [ col1 (str "txs"),
                        col2 (rdToStr txs show)
                      ],
                      -- miner data
                      [ padTop (Pad 2) $ col1 (str "miner"),
                        padTop (Pad 2) $ col2 (rdToStr poolName T.unpack)
                      ],
                      [ col1 (str "fees"),
                        col2 $
                          vBox
                            [ (if st ^. extraInfo then withBold else id) $ rdToStr poolFees (show . toBtc),
                              if st ^. extraInfo then rdToFiatStr poolFees else emptyWidget
                            ]
                      ],
                      [ col1 (str "reward"),
                        col2 $
                          vBox
                            [ (if st ^. extraInfo then withBold else id) $ rdToStr reward (show . toBtc),
                              if st ^. extraInfo then rdToFiatStr reward else emptyWidget
                            ]
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
    rdToStr :: forall a n. (Show a) => (Block -> a) -> (a -> String) -> Widget n
    rdToStr accessor show' =
      let loadingStr = drawLoadingString4 (st ^. tick)
       in case rdBlock of
            NotAsked -> loadingStr
            Loading ma -> maybe loadingStr (str . show' . accessor) ma
            Failure _ -> withError $ str "error"
            Success b -> str $ show' $ accessor b
    rdToFiatStr :: forall n. (Block -> Amount 'SATS) -> Widget n
    rdToFiatStr accessor =
      let errorStr = withError $ str "error"
          priceStr :: Block -> Prices -> Widget n
          priceStr b ps =
            let s = accessor b
             in case st ^. selectedFiat of
                  EUR -> str $ show $ satsToFiat s (pEUR ps)
                  USD -> str $ show $ satsToFiat s (pUSD ps)
                  GBP -> str $ show $ satsToFiat s (pGBP ps)
                  CAD -> str $ show $ satsToFiat s (pCAD ps)
                  CHF -> str $ show $ satsToFiat s (pCHF ps)
                  AUD -> str $ show $ satsToFiat s (pAUD ps)
                  JPY -> str $ show $ satsToFiat s (pJPY ps)
       in case liftA2 (,) rdBlock (st ^. prices) of
            Loading (Just (b, ps)) -> priceStr b ps
            Success (b, ps) -> priceStr b ps
            Failure _ -> errorStr
            _ -> drawLoadingString4 (st ^. tick)

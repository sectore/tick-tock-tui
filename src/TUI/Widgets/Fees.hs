{-# LANGUAGE DataKinds #-}

module TUI.Widgets.Fees (drawFees) where

import Brick.Types
  ( Widget,
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( Padding (..),
    padBottom,
    padLeft,
    padRight,
    str,
    vBox,
    (<+>),
  )
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold, withError)
import TUI.Service.Types (Amount (Amount), Fees (..), FeesRD, Fiat (..), Prices (..), RemoteData (..))
import TUI.Types (TUIResource (..), TUIState (..), fees, prices, selectedFiat, tick)
import TUI.Utils (emptyStr, satsToFiat)
import TUI.Widgets.Loader (drawLoadingString2, drawSpinner)

drawFees :: TUIState -> Widget TUIResource
drawFees st =
  vBox
    [ hCenter $ padBottom (Pad 2) $ withBold $ str "FEES" <+> padLeft (Pad 1) loadingAnimation,
      hCenter $
        renderTable $
          surroundingBorder False $
            rowBorders False $
              columnBorders False $
                setDefaultColAlignment AlignLeft $
                  table
                    [ [ col1Left (str "fast") <+> col1Right (str "~10min"),
                        col2Left $
                          vBox
                            [ withBold
                                ( feesRdStr fast rdFees
                                    <+> col1Right (str " sat/vB")
                                ),
                              fiatPriceStr fast
                            ]
                      ],
                      [ col1Left (str "medium") <+> col1Right (str "~30min"),
                        col2Left $
                          vBox
                            [ withBold
                                ( feesRdStr medium rdFees
                                    <+> col1Right (str " sat/vB")
                                ),
                              fiatPriceStr medium
                            ]
                      ],
                      [ col1Left (str "slow") <+> col1Right (str "~60min"),
                        col2Left $
                          vBox
                            [ withBold
                                ( feesRdStr slow rdFees
                                    <+> col1Right (str " sat/vB")
                                ),
                              fiatPriceStr slow
                            ]
                      ]
                    ]
    ]
  where
    rdFees = st ^. fees
    rdPrices = st ^. prices
    loadingAnimation =
      let spinner = drawSpinner (st ^. tick)
       in case rdFees of
            NotAsked -> spinner
            Loading _ -> spinner
            _ -> emptyStr
    col1Left = withBold . padRight (Pad 1)
    col1Right = padRight (Pad 10)
    col2Left = padLeft (Pad 10) . padRight (Pad 1)
    feesRdStr :: forall a n. (Show a) => (Fees -> a) -> FeesRD -> Widget n
    feesRdStr feeL rd =
      let loadingStr = drawLoadingString2 (st ^. tick)
       in case rd of
            NotAsked -> loadingStr
            Loading mFs -> maybe loadingStr (str . show . feeL) mFs
            Failure _ -> withError $ str "error"
            Success fs -> str $ show $ feeL fs
    fiatPriceStr feeL =
      let errorStr = withError $ str "error"
          priceStr :: Fees -> Prices -> Widget n
          priceStr fs ps =
            -- tx cost in `SATS` based on average txs size of 140 vb
            let s = Amount $ toEnum (feeL fs) * 140
             in case st ^. selectedFiat of
                  EUR -> str $ show $ satsToFiat s (pEUR ps)
                  USD -> str $ show $ satsToFiat s (pUSD ps)
                  GBP -> str $ show $ satsToFiat s (pGBP ps)
                  CAD -> str $ show $ satsToFiat s (pCAD ps)
                  CHF -> str $ show $ satsToFiat s (pCHF ps)
                  AUD -> str $ show $ satsToFiat s (pAUD ps)
                  JPY -> str $ show $ satsToFiat s (pJPY ps)
       in case liftA2 (,) rdFees rdPrices of
            Loading (Just (fs, ps)) -> priceStr fs ps
            Success (fs, ps) -> priceStr fs ps
            Failure _ -> errorStr
            _ -> drawLoadingString2 (st ^. tick)

module TUI.Widgets.Footer where

import Brick.Types (
  Widget,
 )
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Core (
  Padding (..),
  emptyWidget,
  fill,
  hBox,
  padLeft,
  padLeftRight,
  padRight,
  padTop,
  str,
  txt,
  vBox,
  vLimit,
  (<+>),
 )
import Brick.Widgets.Table
import Lens.Micro ((^.))
import TUI.Attr (withBold)
import TUI.Config (Config (cfgMempoolUrl, cfgStorageDirectory))
import TUI.Service.Types (Bitcoin (BTC))
import TUI.Storage (getStoragePath)
import TUI.Types (
  MempoolUrl (..),
  TUIResource (..),
  TUIState,
  View (..),
  animate,
  currentView,
  extraInfo,
  selectedBitcoin,
  showMenu,
 )
import TUI.Widgets.Countdown (drawCountdown)

drawFooter :: TUIState -> Config -> Widget TUIResource
drawFooter st config =
  vBox $
    -- menu title + border + countdown
    [ hBox
        [ padLeftRight 1 $ str $ "[m]enu " <> if st ^. showMenu then "↓" else "↑"
        , vLimit 1 $ fill $ BS.bsHorizontal BS.ascii
        , padLeftRight 1 $ drawCountdown st
        ]
    ]
      -- menu content
      <> ( [ padLeftRight 1 $
            renderTable $
              surroundingBorder False $
                rowBorders False $
                  columnBorders False $
                    setDefaultColAlignment AlignLeft $
                      table
                        ( [ [col1 $ str "screens", views]
                          , [col1 $ str "actions", actions]
                          , [col1 emptyWidget, actions2]
                          ]
                            ++ [ row | st ^. extraInfo, row <-
                                                          [
                                                            [ padTop (Pad 1) $ col1 $ str "endpoint"
                                                            , padTop (Pad 1) $ txt $ unMempoolUrl $ cfgMempoolUrl config
                                                            ]
                                                          ,
                                                            [ col1 $ str "storage"
                                                            , str $ getStoragePath $ cfgStorageDirectory config
                                                            ]
                                                          ]
                               ]
                        )
           | st ^. showMenu
           ]
         )
  where
    col1 = padRight (Pad 6) . withBold
    foldWithSpace = foldl1 (\x y -> x <+> (padLeft $ Pad 2) y)
    viewLabels =
      [ (FeesView, "[f]ees")
      , (BlockView, "[b]lock")
      , (ConverterView, "[c]onverter")
      ]
    v = st ^. currentView
    views =
      foldWithSpace
        -- create a list of view labels
        [ if v' == v
          then -- bold label for `currentView`
            withBold (str label)
          else str label
        | (v', label) <- viewLabels
        ]
    actionLabels =
      [ "[r]eload data"
      , "[s]witch to " ++ if st ^. selectedBitcoin == BTC then "sat" else "btc"
      , "[t]oggle fiat"
      ]
    actionLabels2 =
      [ "[e]xtra info"
      , "[a]nimation " ++ if st ^. animate then "stop" else "start"
      , "[q]uit"
      ]
    actions = foldWithSpace $ str <$> actionLabels
    actions2 = foldWithSpace $ str <$> actionLabels2

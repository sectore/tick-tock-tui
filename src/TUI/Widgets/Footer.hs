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
  changeScreenMode,
  currentView,
  editMode,
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
        , vLimit 1 $ fill $ BS.bsHorizontal BS.defaultBorderStyle
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
                        ( [ -- row: screens

                            [ col1 $ str "screens"
                            , if st ^. changeScreenMode then hBox [str "switch to ", views] else str "[^s]witch screen"
                            ]
                          ]
                            --  row: actions
                            ++ [ [col1 $ str "actions", actions]
                               , [col1 emptyWidget, actions2]
                               ]
                            --  row: actions
                            ++ [ row | v == ConverterView || v == RatioView, row <-
                                                                              [
                                                                                [ col1 $ str "edit"
                                                                                , if not $ st ^. editMode then str "[^e]nter edit mode" else edit
                                                                                ]
                                                                              ]
                               ]
                            -- row: extra info
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
      [ (DashboardView, "[0] dashboard")
      , (FeesView, "[1] fees")
      , (BlockView, "[2] block")
      , (ConverterView, "[3] converter")
      , (RatioView, "[4] ratio")
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
    editLabels =
      [ "[ENTER] apply changes"
      , "[ESC] skip changes"
      ]
    actions = foldWithSpace $ str <$> actionLabels
    actions2 = foldWithSpace $ str <$> actionLabels2
    edit = foldWithSpace $ str <$> editLabels

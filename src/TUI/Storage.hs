module TUI.Storage (
  save,
  load,
  toStorage,
  getStoragePath,
  defaultStorage,
) where

import Brick.Forms (formState)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Lens.Micro (to, (^.))
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
 )
import System.FilePath ((</>))
import TUI.Service.Types (Amount (..), Bitcoin (..), Fiat (..), mkTicker)
import TUI.Types

-- | Default instance
defaultStorage :: TUIStorage
defaultStorage =
  TUIStorage
    { stgCurrentView = FeesView
    , stgAnimate = False
    , stgExtraInfo = False
    , stgSelectedFiat = USD
    , stgShowMenu = False
    , stgSelectedBitcoin = BTC
    , stgBtcAmount = Amount 0.00021
    , stgAssetTicker = mkTicker "ETH"
    }

stgVersion :: Int
stgVersion = 1

toStorage :: TUIState -> TUIStorage
toStorage st =
  TUIStorage
    { stgCurrentView = st ^. currentView
    , stgAnimate = st ^. animate
    , stgExtraInfo = st ^. extraInfo
    , stgSelectedFiat = st ^. selectedFiat
    , stgSelectedBitcoin = st ^. selectedBitcoin
    , stgShowMenu = st ^. showMenu
    , stgBtcAmount = st ^. converterForm . to formState . cdBTC
    , stgAssetTicker = st ^. ratioForm . to formState . rdTicker
    }

getStoragePath :: FilePath -> FilePath
getStoragePath dir = dir </> "data" <> show stgVersion <> ".json"

save :: TUIStorage -> FilePath -> IO ()
save st dir = do
  createDirectoryIfMissing True dir
  LBS.writeFile (getStoragePath dir) (A.encode st)

load :: FilePath -> IO (Maybe TUIStorage)
load dir = do
  let filePath = getStoragePath dir
  exists <- doesFileExist filePath
  if exists
    then A.decode <$> LBS.readFile filePath
    else pure Nothing

module TUI.Storage (save, load, toStorage, getStoragePath) where

import Brick.Forms (formState)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Lens.Micro (to, (^.))
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
 )
import System.FilePath ((</>))
import TUI.Types

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

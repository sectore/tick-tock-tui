{-# LANGUAGE OverloadedStrings #-}

module TUI.Storage (save, load, toStorage) where

import Brick.Forms (formState)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Lens.Micro (to, (^.))
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath ((</>))
import TUI.Types

stgVersion :: Int
stgVersion = 1

toStorage :: TUIState -> TUIStorage
toStorage st =
  TUIStorage
    { stgCurrentView = st ^. currentView,
      stgAnimate = st ^. animate,
      stgExtraInfo = st ^. extraInfo,
      stgSelectedFiat = st ^. selectedFiat,
      stgSelectedBitcoin = st ^. selectedBitcoin,
      stgShowMenu = st ^. showMenu,
      stgBtcAmount = st ^. converterForm . to formState . cdBTC
    }

path :: FilePath -> FilePath
path dir = dir </> "data" <> show stgVersion <> ".json"

-- | Path to store data
-- Note: It uses 'System.Directory.XdgState' to save parts of 'TUIState'
getStorageDirectory :: IO FilePath
getStorageDirectory = getXdgDirectory XdgState "tick-tock-tui"

save :: TUIStorage -> IO ()
save st = do
  dir <- getStorageDirectory
  createDirectoryIfMissing True dir
  BL.writeFile (path dir) (A.encode st)

load :: IO (Maybe TUIStorage)
load = do
  filePath <- path <$> getStorageDirectory
  exists <- doesFileExist filePath
  if exists
    then A.decode <$> BL.readFile filePath
    else pure Nothing

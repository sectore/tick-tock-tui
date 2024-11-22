module TUI.Config where

import Options.Applicative
import System.Directory (
  XdgDirectory (..),
  getXdgDirectory,
 )
import TUI.Types (MempoolUrl (..))

data Config = Config
  { cfgMempoolUrl :: !MempoolUrl
  , cfgReloadInterval :: !Int
  , cfgStorageDirectory :: !FilePath
  }
  deriving (Show)

parser :: FilePath -> Parser Config
parser defaultStorageDirectory =
  Config
    <$> ( MempoolUrl
            <$> strOption
              ( long "mempool"
                  <> short 'm'
                  <> metavar "URL"
                  <> help "Mempool URL"
                  <> value "https://mempool.space"
                  <> showDefault
              )
        )
    <*> option
      auto
      ( long "refresh"
          <> short 'r'
          <> metavar "SECONDS"
          <> help "Interval to auto-reload data in seconds"
          <> value 180
          <> showDefault
      )
    <*> option
      auto
      ( long "storage"
          <> short 's'
          <> metavar "DIRECTORY"
          <> help "Folder to store application state"
          <> value defaultStorageDirectory
          <> showDefault
      )

getConfig :: IO Config
getConfig = do
  defaultStorageDirectory <- getXdgDirectory XdgState "tick-tock-tui"
  execParser (opts defaultStorageDirectory)
  where
    opts :: FilePath -> ParserInfo Config
    opts path =
      info
        (parser path <**> helper)
        ( fullDesc
            <> progDesc "TUI app to handle Bitcoin data provided by Mempool: fees, blocks and price converter."
            <> header "tick-tock-tui"
        )

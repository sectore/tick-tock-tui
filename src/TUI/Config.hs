module TUI.Config where

import Options.Applicative

import TUI.Types (MempoolUrl (..))

data Config = Config
  { cfgMempoolUrl :: !MempoolUrl
  , cfgReloadInterval :: !Int
  , cfgStorageDirectory :: !FilePath
  , cfgIgnoreStorage :: !Bool
  }
  deriving (Show)

parser :: FilePath -> MempoolUrl -> Parser Config
parser defaultStorageDirectory (MempoolUrl defaultMempoolUrl) =
  Config
    . MempoolUrl
    <$> strOption
      ( long "mempool"
          <> short 'm'
          <> metavar "URL"
          <> help "Mempool URL"
          <> value defaultMempoolUrl
          <> showDefault
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
    <*> switch
      ( long "ignore"
          <> short 'i'
          <> help "Ignore previous stored application state to use default data instead."
      )

getConfig :: MempoolUrl -> FilePath -> IO Config
getConfig defaultMempoolUrl defaultStorageDirectory = do
  execParser opts
  where
    opts :: ParserInfo Config
    opts =
      info
        (parser defaultStorageDirectory defaultMempoolUrl <**> helper)
        ( fullDesc
            <> progDesc
              "Get latest Bitcoin data in your terminal: fees, blocks, ratio and prices incl. a price converter. Most data are provided by Mempool. Connect to your own Mempool instance if you like. Latest prices of other assets coming from Krakens Spot API."
            <> header "tick-tock-tui"
        )

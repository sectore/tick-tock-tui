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
    <$> ( MempoolUrl
            <$> strOption
              ( long "mempool"
                  <> short 'm'
                  <> metavar "URL"
                  <> help "Mempool URL"
                  <> value defaultMempoolUrl
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
            <> progDesc "TUI app to handle Bitcoin data provided by Mempool: fees, blocks and price converter."
            <> header "tick-tock-tui"
        )

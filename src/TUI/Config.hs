module TUI.Config where

import Options.Applicative
import TUI.Types (MempoolUrl (..))

data Config = Config
  { cfgMempoolUrl :: !MempoolUrl
  , cfgReloadInterval :: !Int
  }
  deriving (Show)

parser :: Parser Config
parser =
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

getConfig :: IO Config
getConfig = execParser opts
  where
    opts :: ParserInfo Config
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "TUI app to handle Bitcoin data provided by Mempool: fees, blocks and price converter."
            <> header "tick-tock-tui"
        )

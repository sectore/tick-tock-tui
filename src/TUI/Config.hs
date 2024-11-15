module TUI.Config where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import TUI.Types (MempoolUrl (..))

data Config = Config
  { cfgMempoolUrl :: !MempoolUrl
  }
  deriving (Show)

loadConfig :: IO Config
loadConfig = do
  _ <- loadFile Configuration.Dotenv.defaultConfig
  url <- fromMaybe "https://mempool.space" <$> lookupEnv "MEMPOOL_URL"
  pure
    Config
      { cfgMempoolUrl = MempoolUrl $ T.pack url
      }

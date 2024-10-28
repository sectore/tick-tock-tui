module TUI.Config where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

data Config = Config
  { cfgMempoolUrl :: !String
  }
  deriving (Show)

loadConfig :: IO Config
loadConfig = do
  _ <- loadFile Configuration.Dotenv.defaultConfig
  url <- fromMaybe "https://mempool.space/api" <$> lookupEnv "MEMPOOL_API"
  pure
    Config
      { cfgMempoolUrl = url
      }

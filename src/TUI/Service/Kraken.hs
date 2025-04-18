module TUI.Service.Kraken where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Text as T
import qualified TUI.Service.API as API
import TUI.Service.Types (Ticker (..))
import TUI.Types (ServiceEnv (..), ServiceM)

krakenBaseUrl :: T.Text
krakenBaseUrl = "https://api.kraken.com/0/public"

fetchAssetPrice :: Ticker -> ServiceM ()
fetchAssetPrice ticker@(Ticker t) = do
  inCh <- asks envInChan
  liftIO $
    API.fetchAssetPrice
      ticker
      (krakenBaseUrl <> T.pack ("/Ticker?pair=" ++ T.unpack t ++ "USD"))
      inCh

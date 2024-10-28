module TUI.Service.Mempool where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import TUI.Service.API qualified as API
import TUI.Types (ServiceEnv (..), ServiceM)

fetchPrices :: ServiceM ()
fetchPrices = do
  baseUrl <- asks envMempoolUrl
  inCh <- asks envInChan
  liftIO $ API.fetchPrices (baseUrl <> "/api/v1/prices") inCh

fetchFees :: ServiceM ()
fetchFees = do
  baseUrl <- asks envMempoolUrl
  inCh <- asks envInChan
  liftIO $ API.fetchFees (baseUrl <> "/api/v1/fees/recommended") inCh

fetchAllData :: ServiceM ()
fetchAllData = do
  fetchPrices
  fetchFees

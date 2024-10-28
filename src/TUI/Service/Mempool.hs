module TUI.Service.Mempool where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import TUI.Service.API qualified as API
import TUI.Types (ServiceEnv (..), ServiceM)

fetchPrices :: ServiceM ()
fetchPrices = do
  baseUrl <- asks envMempoolUrl
  inCh <- asks envInChan
  liftIO $ API.fetchPrices (baseUrl <> "/v1/api/prices") inCh

fetchFees :: ServiceM ()
fetchFees = do
  baseUrl <- asks envMempoolUrl
  inCh <- asks envInChan
  liftIO $ API.fetchFees (baseUrl <> "/v1/api/fees/recommended") inCh

fetchAllData :: ServiceM ()
fetchAllData = do
  fetchPrices
  fetchFees

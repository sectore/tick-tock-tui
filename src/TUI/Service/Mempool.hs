module TUI.Service.Mempool where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text as T
import TUI.Service.API qualified as API
import TUI.Types (MempoolUrl (..), ServiceEnv (..), ServiceM)

fetchPrices :: ServiceM ()
fetchPrices = do
  (MempoolUrl baseUrl) <- asks envMempoolUrl
  inCh <- asks envInChan
  liftIO $ API.fetchPrices (baseUrl <> T.pack "/api/v1/prices") inCh

fetchFees :: ServiceM ()
fetchFees = do
  (MempoolUrl baseUrl) <- asks envMempoolUrl
  inCh <- asks envInChan
  liftIO $ API.fetchFees (baseUrl <> T.pack "/api/v1/fees/recommended") inCh

fetchBlock :: ServiceM ()
fetchBlock = do
  (MempoolUrl baseUrl) <- asks envMempoolUrl
  inCh <- asks envInChan
  liftIO $ API.fetchBlock (baseUrl <> T.pack "/api/v1/blocks") inCh

fetchAllData :: ServiceM ()
fetchAllData = do
  fetchPrices
  fetchFees
  fetchBlock
  pure ()

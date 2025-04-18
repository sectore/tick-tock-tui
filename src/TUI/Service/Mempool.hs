module TUI.Service.Mempool where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Text as T
import qualified TUI.Service.API as API
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

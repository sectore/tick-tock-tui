module TUI.Service.Mempool where

import TUI.Service.API qualified as API
import GHC.Conc (ThreadId)
import Brick.BChan (BChan)
import TUI.Types (TUIEvent)
import Control.Concurrent (forkIO)

fetchPrices :: BChan TUIEvent -> IO ()
fetchPrices = API.fetchPrices "https://mempool.space/api/v1/prices"

fetchFees :: BChan TUIEvent -> IO ()
fetchFees = API.fetchFees "https://mempool.space/api/v1/fees/recommended"

fetchAllData :: BChan TUIEvent -> IO [ThreadId]
fetchAllData inCh = do
  pId <- forkIO $ fetchPrices inCh
  fId <- forkIO $ fetchFees inCh
  pure [pId, fId]

module TUI.Service.Common where

import qualified TUI.Service.Kraken as K
import qualified TUI.Service.Mempool as M
import TUI.Service.Types (Ticker (..))
import TUI.Types

fetchAllData :: Ticker -> ServiceM ()
fetchAllData ticker = do
  M.fetchPrices
  M.fetchFees
  M.fetchBlock
  K.fetchAssetPrice ticker
  pure ()

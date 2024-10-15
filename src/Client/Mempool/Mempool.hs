module Client.Mempool.Mempool where

import Brick.BChan
import Client.Types (ClientEvent)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM qualified as STM
import Control.Monad (forever)
import TUI.Types

mkClient :: BChan TUIEvent -> STM.TChan ClientEvent -> IO ()
mkClient tChan cChan = do
    _ <- w
    _ <- r
    pure ()
  where
    r = forever $ do
        e <- STM.atomically $ STM.readTChan cChan
        print e
    w =
        forever $
            do
                writeBChan tChan Counter
                threadDelay 1000000

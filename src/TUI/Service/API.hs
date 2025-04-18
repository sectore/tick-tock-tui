module TUI.Service.API where

import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Internal (tryAll)
import Control.Exception (try)
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client.Conduit (HttpException, parseRequest)
import Network.HTTP.Simple (getResponseBody, httpLBS)
import TUI.Service.Types (RemoteData (..), Ticker (..))
import TUI.Types

fetchData :: (A.FromJSON a) => Text -> IO (Either String a)
fetchData url = do
  request <- parseRequest $ "GET " ++ T.unpack url
  result <- try $ httpLBS request
  case result of
    Left e -> return $ Left $ "HTTP exception: " ++ show (e :: HttpException)
    Right response -> do
      let body = getResponseBody response
      case A.eitherDecode body of
        Left err -> return $ Left $ "JSON parsing error: " ++ err
        Right value -> return $ Right value

fetchAndNotify
  :: (A.FromJSON a)
  => (RemoteData String a -> TUIEvent)
  -> Text
  -> BChan TUIEvent
  -> IO ()
fetchAndNotify toEvent url inCh = do
  -- TODO: Remove delay
  threadDelay 1_000_000
  result <- tryAll $ fetchData url
  rd <- case result of
    Left e -> return $ Failure $ "Async error: " ++ show e
    Right (Left err) -> return $ Failure $ "HTTP/Parsing error: " ++ err
    Right (Right v) -> return $ Success v
  writeBChan inCh (toEvent rd)

fetchPrices :: Text -> BChan TUIEvent -> IO ()
fetchPrices = fetchAndNotify PriceUpdated

fetchFees :: Text -> BChan TUIEvent -> IO ()
fetchFees = fetchAndNotify FeesUpdated

fetchBlock :: Text -> BChan TUIEvent -> IO ()
fetchBlock = fetchAndNotify BlockUpdated

fetchAssetPrice :: Ticker -> Text -> BChan TUIEvent -> IO ()
fetchAssetPrice ticker = fetchAndNotify (AssetPriceUpdated ticker)

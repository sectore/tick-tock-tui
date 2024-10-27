{-# LANGUAGE DataKinds #-}

module TUI.Service.API where

import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async.Internal (tryAll)
import Control.Exception (try)
import Data.Aeson qualified as A
import GHC.Conc (ThreadId)
import Network.HTTP.Client.Conduit (HttpException, parseRequest)
import Network.HTTP.Simple (getResponseBody, httpLBS)
import TUI.Service.Types (RemoteData (..))
import TUI.Types

fetchData :: (A.FromJSON a) => String -> IO (Either String a)
fetchData url = do
  request <- parseRequest $ "GET " ++ url
  result <- try $ httpLBS request
  case result of
    Left e -> return $ Left $ "HTTP exception: " ++ show (e :: HttpException)
    Right response -> do
      let body = getResponseBody response
      case A.eitherDecode body of
        Left err -> return $ Left $ "JSON parsing error: " ++ err
        Right value -> return $ Right value

fetchPrices :: String -> BChan TUIEvent -> IO ()
fetchPrices url inCh = do
  -- TODO: Remove delay
  threadDelay 1_000_000
  priceResult <- tryAll $ fetchData url
  rd <- case priceResult of
    Left e -> return $ Failure $ "Async error: " ++ show e
    Right (Left err) -> return $ Failure $ "HTTP/Parsing error: " ++ err
    Right (Right pd) -> return $ Success pd
  writeBChan inCh (PriceUpdated rd)

fetchFees :: String -> BChan TUIEvent -> IO ()
fetchFees url inCh = do
  -- TODO: Remove delay
  threadDelay 1_000_000
  feesResult <- tryAll $ fetchData url
  rd <- case feesResult of
    Left e -> return $ Failure $ "Async error: " ++ show e
    Right (Left err) -> return $ Failure $ "HTTP/Parsing error: " ++ err
    Right (Right pd) -> return $ Success pd
  writeBChan inCh (FeesUpdated rd)

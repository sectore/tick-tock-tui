{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Client.Client where

import Client.Types (Currency (..), Price (..), RemoteData)
import Control.Exception (try)
import Data.Aeson
import Network.HTTP.Client.Conduit (HttpException, parseRequest)
import Network.HTTP.Simple (getResponseBody, httpLBS)

data Prices = Prices
  { eur :: Price EUR,
    usd :: Price USD
  }
  deriving (Show, Eq)

instance FromJSON Prices where
  parseJSON (Object o) =
    Prices
      <$> (Price <$> o .: "EUR")
      <*> (Price <$> o .: "USD")
  parseJSON v = fail $ "Could not parse PriceData from " ++ show v

type PricesRD = RemoteData String Prices

data Fees = Fees
  { fast :: Int,
    medium :: Int,
    slow :: Int
  }
  deriving (Show, Eq)

type FeesRD = RemoteData String Fees

instance FromJSON Fees where
  parseJSON (Object o) =
    Fees
      <$> o .: "fastestFee"
      <*> o .: "halfHourFee"
      <*> o .: "hourFee"
  parseJSON v = fail $ "Could not parse Fees from " ++ show v

fetchData :: (FromJSON a) => String -> IO (Either String a)
fetchData url = do
  request <- parseRequest $ "GET " ++ url
  result <- try $ httpLBS request
  case result of
    Left e -> return $ Left $ "HTTP exception: " ++ show (e :: HttpException)
    Right response -> do
      let body = getResponseBody response
      case eitherDecode body of
        Left err -> return $ Left $ "JSON parsing error: " ++ err
        Right value -> return $ Right value

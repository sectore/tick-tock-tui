{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.Service.Types where

import Data.Aeson
import Text.Printf (printf)

data Bitcoin = BTC | SATS
  deriving (Eq)

data Fiat = EUR | USD | GBP | CAD | CHF | AUD | JPY
  deriving (Eq, Enum, Bounded)

newtype Price (a :: Fiat) = Price {unPrice :: Float}
  deriving (Eq)

showFiat :: Fiat -> Float -> String
showFiat fiat = printf (fiatSymbol fiat <> "%.2f")

fiatSymbol :: Fiat -> String
fiatSymbol = \case
  EUR -> "€"
  USD -> "$"
  GBP -> "£"
  CAD -> "C$"
  CHF -> "CHF"
  AUD -> "A$"
  JPY -> "¥"

instance Show (Price 'EUR) where
  show = showFiat EUR . unPrice

instance Show (Price 'USD) where
  show = showFiat USD . unPrice

instance Show (Price 'GBP) where
  show = showFiat GBP . unPrice

instance Show (Price 'CAD) where
  show = showFiat CAD . unPrice

instance Show (Price 'CHF) where
  show = showFiat CHF . unPrice

instance Show (Price 'AUD) where
  show = showFiat AUD . unPrice

instance Show (Price 'JPY) where
  show = showFiat JPY . unPrice

data Prices = Prices
  { pEUR :: Price EUR,
    pUSD :: Price USD,
    pGBP :: Price GBP,
    pCAD :: Price CAD,
    pCHF :: Price CHF,
    pAUD :: Price AUD,
    pJPY :: Price JPY
  }
  deriving (Show, Eq)

instance FromJSON Prices where
  parseJSON (Object o) =
    Prices
      <$> (Price <$> o .: "EUR")
      <*> (Price <$> o .: "USD")
      <*> (Price <$> o .: "GBP")
      <*> (Price <$> o .: "CAD")
      <*> (Price <$> o .: "CHF")
      <*> (Price <$> o .: "AUD")
      <*> (Price <$> o .: "JPY")
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

newtype Amount a = Amount {unAmount :: Float}
  deriving (Eq)

instance Show (Amount 'USD) where
  show = showFiat USD . unAmount

instance Show (Amount 'EUR) where
  show = showFiat EUR . unAmount

instance Show (Amount 'GBP) where
  show = showFiat GBP . unAmount

instance Show (Amount 'CHF) where
  show = showFiat CHF . unAmount

instance Show (Amount 'CAD) where
  show = showFiat CAD . unAmount

instance Show (Amount 'AUD) where
  show = showFiat AUD . unAmount

instance Show (Amount 'JPY) where
  show = showFiat JPY . unAmount

instance Show (Amount 'BTC) where
  show = printf "%.8f ₿" . unAmount

instance Show (Amount 'SATS) where
  show = printf "%.0f sat" . unAmount

data RemoteData e a
  = NotAsked
  | Loading (Maybe a)
  | Failure e
  | Success a
  deriving (Show, Eq)

isLoading :: RemoteData e a -> Bool
isLoading (Loading _) = True
isLoading _ = False

data ApiEvent = FetchAllData | FetchPrices | FetchFees
  deriving (Eq, Show)

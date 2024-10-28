{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.Service.Types where

import Data.Aeson
import Text.Printf (printf)

data Bitcoin = BTC | SATS
  deriving (Eq)

data Fiat = FiatEUR | FiatUSD | FiatGBP | FiatCAD | FiatCHF | FiatAUD | FiatJPY
  deriving (Eq, Enum, Bounded)

newtype Price (a :: Fiat) = Price {unPrice :: Float}
  deriving (Eq)

showFiat :: Fiat -> Float -> String
showFiat fiat = printf (fiatSymbol fiat <> "%.0f")

fiatSymbol :: Fiat -> String
fiatSymbol = \case
  FiatEUR -> "€"
  FiatUSD -> "$"
  FiatGBP -> "£"
  FiatCAD -> "C$"
  FiatCHF -> "CHF"
  FiatAUD -> "A$"
  FiatJPY -> "¥"

instance Show (Price 'FiatEUR) where
  show = showFiat FiatEUR . unPrice

instance Show (Price 'FiatUSD) where
  show = showFiat FiatUSD . unPrice

instance Show (Price 'FiatGBP) where
  show = showFiat FiatGBP . unPrice

instance Show (Price 'FiatCAD) where
  show = showFiat FiatCAD . unPrice

instance Show (Price 'FiatCHF) where
  show = showFiat FiatCHF . unPrice

instance Show (Price 'FiatAUD) where
  show = showFiat FiatAUD . unPrice

instance Show (Price 'FiatJPY) where
  show = showFiat FiatJPY . unPrice

-- Sometimes `Fiat` is used as type-level value (e.g. `Price (a :: Fiat)` in  `Service.Types`),
-- but also as runtime value (e.g. `selectedFiat` in `TUIState`)
-- Both can't be used in a function w/o some extra effort (via TypeFamilies etc.)
-- That's a simple GADT approach might help here
data WPrice where
  WPrice :: forall (f :: Fiat). (Show (Price f)) => Price f -> WPrice

instance Show WPrice where
  show (WPrice p) = show p

unWPrice :: WPrice -> Float
unWPrice (WPrice p) = unPrice p

data Prices = Prices
  { pEUR :: Price FiatEUR,
    pUSD :: Price FiatUSD,
    pGBP :: Price FiatGBP,
    pCAD :: Price FiatCAD,
    pCHF :: Price FiatCHF,
    pAUD :: Price FiatAUD,
    pJPY :: Price FiatJPY
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

class GetPrice (f :: Fiat) where
  getPrice :: Fiat -> Prices -> Price f

instance GetPrice FiatEUR where
  getPrice _ = pEUR

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

newtype Amount (a :: k) = Amount {unAmount :: Float}
  deriving (Eq)

instance Show (Amount 'FiatUSD) where
  show = showFiat FiatUSD . unAmount

instance Show (Amount 'FiatEUR) where
  show = showFiat FiatEUR . unAmount

instance Show (Amount 'FiatGBP) where
  show = showFiat FiatGBP . unAmount

instance Show (Amount 'FiatCHF) where
  show = showFiat FiatCHF . unAmount

instance Show (Amount 'FiatCAD) where
  show = showFiat FiatCAD . unAmount

instance Show (Amount 'FiatAUD) where
  show = showFiat FiatAUD . unAmount

instance Show (Amount 'FiatJPY) where
  show = showFiat FiatJPY . unAmount

instance Show (Amount 'BTC) where
  show = printf "%.8f ₿" . unAmount

instance Show (Amount 'SATS) where
  show = printf "%.0f sat" . unAmount

class FiatConversion (f :: Fiat) where
  fiatToBtc :: Amount f -> Price f -> Amount BTC
  fiatToSats :: Amount f -> Price f -> Amount SATS

class BitcoinConversion (a :: Bitcoin) where
  toBtc :: Amount a -> Amount BTC
  toSats :: Amount a -> Amount SATS

instance BitcoinConversion BTC where
  toBtc = id
  toSats (Amount a) = Amount $ a * 100000000

instance BitcoinConversion SATS where
  toBtc (Amount a) = Amount $ a / 100_000_000
  toSats = id

-- helper to convert Fiat to BTC
fiatToBtc' :: Amount a -> Price a -> Amount BTC
fiatToBtc' (Amount a) (Price p) = Amount $ a / p

-- helper to convert Fiat to Sats
fiatToSats' :: forall (a :: Fiat). (FiatConversion a) => Amount a -> Price a -> Amount SATS
fiatToSats' a p = toSats $ fiatToBtc a p

instance FiatConversion FiatEUR where
  fiatToBtc = fiatToBtc'
  fiatToSats = fiatToSats'

instance FiatConversion FiatUSD where
  fiatToBtc = fiatToBtc'
  fiatToSats = fiatToSats'

instance FiatConversion FiatGBP where
  fiatToBtc = fiatToBtc'
  fiatToSats = fiatToSats'

instance FiatConversion FiatCAD where
  fiatToBtc = fiatToBtc'
  fiatToSats = fiatToSats'

instance FiatConversion FiatCHF where
  fiatToBtc = fiatToBtc'
  fiatToSats = fiatToSats'

instance FiatConversion FiatAUD where
  fiatToBtc = fiatToBtc'
  fiatToSats = fiatToSats'

instance FiatConversion FiatJPY where
  fiatToBtc = fiatToBtc'
  fiatToSats = fiatToSats'

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

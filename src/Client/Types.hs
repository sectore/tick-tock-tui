{-# LANGUAGE DataKinds #-}

module Client.Types where

import Text.Printf (printf)

data Currency = BTC | SATS | EUR | USD

newtype Amount (a :: Currency) = Amount {unAmount :: Float}
  deriving (Eq)

instance Show (Amount 'USD) where
  show = printf "$ %.2f" . unAmount

instance Show (Amount 'EUR) where
  show = printf "%.2f €" . unAmount

instance Show (Amount 'BTC) where
  show = printf "%.8f ₿" . unAmount

instance Show (Amount 'SATS) where
  show = printf "%.0f sat" . unAmount

newtype Price a = Price {unPrice :: Float}
  deriving (Show, Eq)

class Conversion (a :: Currency) where
  toBtc :: Amount a -> Price a -> Amount BTC
  toSats :: Amount a -> Price a -> Amount SATS

instance Conversion BTC where
  toBtc a _ = a
  toSats (Amount a) _ = Amount $ a * 100000000

instance Conversion SATS where
  toBtc (Amount a) _ = Amount $ a / 100_000_000
  toSats a _ = a

-- helper to convert Fiat to BTC
fiatToBtc :: Amount a -> Price a -> Amount BTC
fiatToBtc (Amount a) (Price p) = Amount $ a / p

-- helper to convert Fiat to Sats
fiatToSats :: Amount a -> Price a -> Amount SATS
fiatToSats a p = flip toSats (Price 1) $ fiatToBtc a p

instance Conversion EUR where
  toBtc = fiatToBtc
  toSats = fiatToSats

instance Conversion USD where
  toBtc = fiatToBtc
  toSats = fiatToSats

data RemoteData e a
  = NotAsked
  | Loading (Maybe a)
  | Failure e
  | Success a
  deriving (Show, Eq)

data ClientEvent = FetchData
  deriving (Eq, Show)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.Service.Types where

import Data.Aeson ((.:))
import Data.Aeson qualified as A
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Text.Printf (printf)

data Bitcoin = BTC | SATS
  deriving (Eq)

data Fiat = EUR | USD | GBP | CAD | CHF | AUD | JPY
  deriving (Eq, Enum, Bounded)

newtype Price (a :: Fiat) = Price {unPrice :: Float}
  deriving (Eq)

showFiatAmount :: Float -> String
showFiatAmount x =
  let s = printf "%.2f" x -- explicitly use 2 decimal places for fiat
      trimmed = reverse . dropWhile (== '0') . reverse $ s
   in if last trimmed == '.'
        then init trimmed -- remove trailing decimal point
        else trimmed

showFiat :: Fiat -> Float -> String
showFiat fiat a = fiatSymbol fiat <> " " <> showFiatAmount a

fiatSymbol :: Fiat -> String
fiatSymbol = \case
  EUR -> "EUR"
  USD -> "USD"
  GBP -> "GBP"
  CAD -> "CAD"
  CHF -> "CHF"
  AUD -> "AUD"
  JPY -> "JPY"

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

instance A.FromJSON Prices where
  parseJSON (A.Object o) =
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

instance A.FromJSON Fees where
  parseJSON (A.Object o) =
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
  show a =
    let str = printf "BTC %.8f" (unAmount a)
        (whole, dec) = break (== '.') (drop 4 str) -- drop "BTC " prefix
        formatGroups [] = []
        formatGroups xs =
          let (g, rest) = splitAt 3 xs
           in g : formatGroups rest
        formatted = case dec of
          (d : ds) -> whole ++ [d] ++ unwords (formatGroups ds)
          [] -> whole
     in "BTC " ++ formatted

instance Show (Amount 'SATS) where
  show = printf "%.0f sats" . unAmount

readAmount :: String -> [(Amount a, String)]
readAmount numberStr = case reads numberStr of
  [(n, r)] -> [(Amount n, r)]
  _ -> []

readFiatAmount :: forall (a :: Fiat). (Show (Amount a)) => String -> [(Amount a, String)]
readFiatAmount str = case words str of
  [code, numberStr]
    | length code == 3 && code == expectedCode -> readAmount numberStr
  _ -> []
  where
    -- Get currency code (first three letters) from `Show` instance
    -- to compare it with `code` parsed above
    expectedCode = case show (undefined :: Amount a) of
      (c1 : c2 : c3 : _) -> [c1, c2, c3]
      _ -> ""

instance Read (Amount 'USD) where
  readsPrec _ = readFiatAmount

instance Read (Amount 'EUR) where
  readsPrec _ = readFiatAmount

instance Read (Amount 'GBP) where
  readsPrec _ = readFiatAmount

instance Read (Amount 'CHF) where
  readsPrec _ = readFiatAmount

instance Read (Amount 'CAD) where
  readsPrec _ = readFiatAmount

instance Read (Amount 'AUD) where
  readsPrec _ = readFiatAmount

instance Read (Amount 'JPY) where
  readsPrec _ = readFiatAmount

readBitcoinAmount :: forall (a :: Bitcoin). String -> [(Amount a, String)]
readBitcoinAmount str = case words str of
  ("BTC" : rest) -> readAmount $ filter (/= ' ') (unwords rest) -- remove all spaces
  [numberStr, "sats"] -> readAmount numberStr
  _ -> []

instance Read (Amount 'BTC) where
  readsPrec _ = readBitcoinAmount

instance Read (Amount 'SATS) where
  readsPrec _ = readBitcoinAmount

data Block = Block
  { time :: UTCTime,
    height :: Int,
    txs :: Int,
    size :: Int,
    poolName :: Text,
    poolFees :: Amount SATS,
    reward :: Amount SATS
  }
  deriving (Show, Eq)

type BlockRD = RemoteData String Block

instance A.FromJSON Block where
  parseJSON = A.withArray "Blocks" $ \arr ->
    case toList arr of
      (firstBlock : _) ->
        A.withObject
          "Block"
          ( \o -> do
              timestamp <- o .: "timestamp" :: Parser Integer
              height <- o .: "height"
              size <- o .: "size"
              txs <- o .: "tx_count"
              extras <- o .: "extras"
              fees <- extras .: "expectedFees"
              pool <- extras .: "pool"
              name <- pool .: "name"
              reward <- extras .: "reward"
              pure
                Block
                  { height = height,
                    txs = txs,
                    size = size,
                    time = posixSecondsToUTCTime (fromIntegral timestamp),
                    poolName = name,
                    poolFees = Amount fees,
                    reward = Amount reward
                  }
          )
          firstBlock
      [] -> fail "Empty array of blocks"

data RemoteData e a
  = NotAsked
  | Loading (Maybe a)
  | Failure e
  | Success a
  deriving (Show, Eq)

isLoading :: RemoteData e a -> Bool
isLoading (Loading _) = True
isLoading _ = False

isSuccess :: RemoteData e a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

data ApiEvent
  = FetchAllData
  | FetchPrices
  | FetchFees
  | FetchBlock
  deriving (Eq, Show)

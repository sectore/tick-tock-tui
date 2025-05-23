module TUI.Service.Types where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as A
import Data.Char (isLetter, toUpper)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate, unfoldr)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Text.Printf (PrintfType, printf)
import Text.Read (readMaybe)

-- | Helper to break down lists into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)

data Bitcoin = BTC | SATS
  deriving (Eq, Show, Generic)

instance A.ToJSON Bitcoin

instance A.FromJSON Bitcoin

data Fiat = EUR | USD | GBP | CAD | CHF | AUD | JPY
  deriving (Eq, Enum, Bounded, Show, Generic)

instance A.ToJSON Fiat

instance A.FromJSON Fiat

newtype Price (a :: Fiat) = Price {unPrice :: Double}
  deriving (Eq)

-- | Helper to print `Fiat` values with precision of 2 decimal
printFiatValue :: (PrintfType r) => Double -> r
printFiatValue = printf "%.2f"

-- | Helper to print `BTC` values with precision of 8 decimal
printBitcoinValue :: (PrintfType r) => Double -> r
printBitcoinValue = printf "%.8f"

-- | Helper to print `SATS` values without any decimal
printSatsValue :: (PrintfType r) => Double -> r
printSatsValue = printf "%.0f"

-- | Helper to format a whole number string with thousand separators
formatThousands :: String -> String
formatThousands = reverse . intercalate "," . chunksOf 3 . reverse

-- Show `Fiat` value with symbol
showFiat :: Fiat -> Double -> String
showFiat fiat a = formatFiat (printFiatValue a) <> " " <> fiatSymbol fiat
  where
    formatFiat str =
      let (whole, dec) = break (== '.') str
       in formatWhole whole <> dec

    -- Format whole number part with thousand separators
    formatWhole whole =
      let normalized = case dropWhile (== '0') whole of
            "" -> "0" -- keep one zero if all zeros
            xs -> xs
       in formatThousands normalized

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
  { pTime :: UTCTime
  , pEUR :: Price EUR
  , pUSD :: Price USD
  , pGBP :: Price GBP
  , pCAD :: Price CAD
  , pCHF :: Price CHF
  , pAUD :: Price AUD
  , pJPY :: Price JPY
  }
  deriving (Show, Eq, Generic)

instance A.FromJSON Prices where
  parseJSON = A.withObject "Prices" $ \o -> do
    timestamp <- o .: "time" :: Parser Integer
    pEUR <- Price <$> o .: "EUR"
    pUSD <- Price <$> o .: "USD"
    pGBP <- Price <$> o .: "GBP"
    pCAD <- Price <$> o .: "CAD"
    pCHF <- Price <$> o .: "CHF"
    pAUD <- Price <$> o .: "AUD"
    pJPY <- Price <$> o .: "JPY"
    pure
      Prices
        { pTime = posixSecondsToUTCTime (fromIntegral timestamp)
        , pEUR = pEUR
        , pUSD = pUSD
        , pGBP = pGBP
        , pCAD = pCAD
        , pCHF = pCHF
        , pAUD = pAUD
        , pJPY = pJPY
        }

type PricesRD = RemoteData String Prices

data Fees = Fees
  { fast :: Int
  , medium :: Int
  , slow :: Int
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

newtype Amount a = Amount {unAmount :: Double}
  deriving (Eq)

instance A.FromJSON (Amount a) where
  parseJSON v = Amount <$> A.parseJSON v

instance A.ToJSON (Amount a) where
  toJSON (Amount x) = A.toJSON x

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
  show (Amount a) = formatBitcoin (printBitcoinValue a) <> " BTC"
    where
      formatBitcoin str =
        let (whole, dec) = break (== '.') str
            withCommas = formatThousands whole
            decimalPart = case dec of
              ('.' : ds) -> formatDecimals ds
              _ -> ""
         in withCommas <> decimalPart

      -- Format decimal part with BTC-specific grouping (2+3+3)
      formatDecimals ds =
        let (first2, rest) = splitAt 2 ds
            restGrouped = unwords (chunksOf 3 rest)
            padding = replicate (6 - length rest) '0' -- pad to 8 decimals total
         in "."
              <> first2
              <> if null rest
                then " " <> chunksToGroups padding -- "000 000"
                else " " <> restGrouped

      -- Helper to convert a string of zeros into groups of three
      chunksToGroups = unwords . chunksOf 3

instance Show (Amount 'SATS) where
  show (Amount a) = formatSats (printSatsValue a) <> " sat"
    where
      formatSats str =
        let normalized = case dropWhile (== '0') str of
              "" -> "0" -- keep one zero if all zeros
              xs -> xs
         in formatThousands normalized

readAmount :: String -> [(Amount a, String)]
readAmount numberStr = case reads numberStr of
  [(n, r)] -> [(Amount n, r)]
  _ -> []

readFiatAmount :: forall (a :: Fiat). (Show (Amount a)) => String -> [(Amount a, String)]
readFiatAmount str = case words str of
  [numberStr, code]
    | length code == 3 && code == expectedCode ->
        let withoutCommas = filter (/= ',') numberStr -- remove commas before parsing
         in readAmount withoutCommas -- parse the cleaned number
  _ -> []
  where
    -- Get currency code (last three letters) from `Show` instance
    -- to compare it with `code` parsed above
    expectedCode = case show (Amount 0 :: Amount a) of
      xs -> drop (length xs - 3) xs

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
readBitcoinAmount str = case reverse $ words str of
  ("BTC" : rest) ->
    let numberStr = unwords $ reverse rest
        withoutCommas = filter (/= ',') numberStr -- remove only commas
        withoutSpaces = filter (/= ' ') withoutCommas -- then remove spaces
     in readAmount withoutSpaces
  ("sat" : rest) ->
    let numberStr = unwords $ reverse rest
        withoutCommas = filter (/= ',') numberStr -- remove commas
     in readAmount withoutCommas
  _ -> []

instance Read (Amount 'BTC) where
  readsPrec _ = readBitcoinAmount

instance Read (Amount 'SATS) where
  readsPrec _ = readBitcoinAmount

data Block = Block
  { time :: UTCTime
  , height :: Int
  , txs :: Int
  , size :: Int
  , poolName :: T.Text
  , poolFees :: Maybe (Amount SATS)
  , reward :: Amount SATS
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
              fees <- extras .:? "expectedFees"
              pool <- extras .: "pool"
              name <- pool .: "name"
              reward <- extras .: "reward"
              pure
                Block
                  { height = height
                  , txs = txs
                  , size = size
                  , time = posixSecondsToUTCTime (fromIntegral timestamp)
                  , poolName = name
                  , poolFees = Amount <$> fees
                  , reward = Amount reward
                  }
          )
          firstBlock
      [] -> fail "Empty array of blocks"

newtype Ticker = Ticker T.Text
  deriving (Eq, Generic)

unTicker :: Ticker -> T.Text
unTicker (Ticker text) = text

tickerToString :: Ticker -> String
tickerToString = T.unpack . unTicker

mkTicker :: String -> Ticker
mkTicker = Ticker . T.pack

instance Show Ticker where
  show = map toUpper . tickerToString

instance Read Ticker where
  readsPrec _ str
    | length str == 3 || length str == 4
    , all isLetter str =
        [(mkTicker (map toUpper str), "")]
    | otherwise = []

instance A.ToJSON Ticker
instance A.FromJSON Ticker

type AssetPriceRD = RemoteData String (Price USD)

-- Price USD is fetched by Krakans API
instance A.FromJSON (Price USD) where
  parseJSON = A.withObject "result" $ \obj -> do
    mResult <- obj .:? "result" :: A.Parser (Maybe (HM.HashMap T.Text A.Value))
    mError <- obj .:? "error" :: A.Parser (Maybe [T.Text])
    case (mResult, mError) of
      (Just resultObj, _) ->
        case HM.elems resultObj of
          -- one asset is expected in `result` only
          (asset : _) -> do
            -- first string in list is the price
            (priceStr : _) <- A.withObject "prices" (.: "c") asset
            case readMaybe priceStr of
              Just p -> return $ Price p
              Nothing -> fail $ "Failed to parse price from: " ++ priceStr
          _ -> fail "No asset found in result"
      (_, Just e) -> fail . T.unpack $ T.concat e
      _ -> fail "unknown error"

data RemoteData e a
  = NotAsked
  | Loading (Maybe a)
  | Failure e
  | Success a
  deriving (Show, Eq)

instance Functor (RemoteData e) where
  fmap _ NotAsked = NotAsked
  fmap f (Loading ma) = Loading (fmap f ma)
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Applicative (RemoteData e) where
  pure = Success

  NotAsked <*> _ = NotAsked
  _ <*> NotAsked = NotAsked
  Failure e <*> _ = Failure e
  _ <*> Failure e = Failure e
  Loading ma <*> rb = Loading (ma <*> toMaybe rb)
    where
      toMaybe (Loading (Just x)) = Just x
      toMaybe (Success x) = Just x
      toMaybe _ = Nothing
  Success f <*> rb = fmap f rb

isLoading :: RemoteData e a -> Bool
isLoading (Loading _) = True
isLoading _ = False

isSuccess :: RemoteData e a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

data ApiEvent
  = FetchAllData Ticker
  | FetchPrices
  | FetchFees
  | FetchBlock
  | FetchAssetPrice Ticker
  deriving (Eq, Show)

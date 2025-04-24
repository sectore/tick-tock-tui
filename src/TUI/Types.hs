{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI.Types where

import Brick (EventM)
import Brick.BChan (BChan)
import Brick.Forms (Form)

import Control.Concurrent.STM (TChan)
import Control.Monad.Reader (ReaderT)
import qualified Data.Aeson as A
import Data.Text (Text)
import Data.Time.LocalTime (TimeZone)
import GHC.Generics (Generic)
import Lens.Micro (Getting, to)
import Lens.Micro.TH (makeLenses)
import TUI.Service.Types (
  Amount,
  ApiEvent,
  AssetPriceRD,
  Bitcoin (..),
  BlockRD,
  FeesRD,
  Fiat (..),
  PricesRD,
  Ticker,
 )

-- Resource names (needed to handle events of form elements etc.)
-- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#resource-names
data TUIResource
  = ConverterFiatField
  | ConverterBtcField
  | ConverterSatField
  | RatioTickerField
  deriving (Eq, Ord, Show)

newtype AppEventEnv = AppEventEnv
  { outChan :: TChan ApiEvent
  }

newtype MempoolUrl = MempoolUrl {unMempoolUrl :: Text}
  deriving (Show)

data ServiceEnv = ServiceEnv
  { envMempoolUrl :: !MempoolUrl
  , envInChan :: BChan TUIEvent
  }

type ServiceM a = ReaderT ServiceEnv IO a

class HasTickEvent e where
  tickEvent :: e

data TickEvent
  = Tick
  deriving (Show)

instance HasTickEvent TickEvent where
  tickEvent = Tick

instance HasTickEvent TUIEvent where
  tickEvent = FPSTick

data TUIEvent
  = PriceUpdated PricesRD
  | FeesUpdated FeesRD
  | BlockUpdated BlockRD
  | AssetPriceUpdated Ticker AssetPriceRD
  | FPSTick
  deriving (Show, Eq)

data ConverterData = ConverterData
  { _cdSelectedFiat :: Fiat
  -- ^ selected `Fiat` value in form
  , _cdSelectedBitcoin :: Bitcoin
  -- ^ selected `Bitcoin` value in form
  , -- `Amount`s of all currencies
    _cdUsd :: Amount 'USD
  , _cdCAD :: Amount 'CAD
  , _cdEUR :: Amount 'EUR
  , _cdGBP :: Amount 'GBP
  , _cdCHF :: Amount 'CHF
  , _cdJPY :: Amount 'JPY
  , _cdAUD :: Amount 'AUD
  , _cdBTC :: Amount 'BTC
  , _cdSATS :: Amount 'SATS
  }
  deriving (Eq, Show)

makeLenses ''ConverterData

data ConverterField
  = FiatField
  | BTCField
  | SatsField
  deriving (Eq, Ord, Show)

type ConverterForm = Form ConverterData TUIEvent TUIResource

newtype RatioData = RatioData
  { _rdTicker :: Ticker
  }
  deriving (Eq, Show)

makeLenses ''RatioData

type RatioForm = Form RatioData TUIEvent TUIResource

data View = FeesView | BlockView | ConverterView | RatioView | DashboardView
  deriving (Eq, Show, Generic)

instance A.FromJSON View

instance A.ToJSON View

data TUIState = TUIState
  { timeZone' :: TimeZone
  -- ^ private
  -- Never get/set value from/to `maxFetchTick'` directly.
  -- Use `maxFetchTick` (without `'`) to read data
  , _currentView :: View
  , _converterForm :: ConverterForm
  , _prevConverterForm :: Maybe ConverterForm
  , _ratioForm :: RatioForm
  , _prevRatioForm :: Maybe RatioForm
  , _animate :: Bool
  , _extraInfo :: Bool
  , _tick :: Int
  , _fetchTick :: Int
  , _lastFetchTick :: Int
  , maxFetchTick' :: Int
  -- ^ private
  -- Never get/set value from/to `maxFetchTick'` directly.
  -- Use `maxFetchTick` (without `'`) to read data
  , _prices :: PricesRD
  , _fees :: FeesRD
  , _block :: BlockRD
  , _assetPrice :: AssetPriceRD
  , _selectedFiat :: Fiat
  , _selectedBitcoin :: Bitcoin
  , _showMenu :: Bool
  , _editMode :: Bool
  }

makeLenses ''TUIState

{- | maxFetchTick lens
custom getter to provide a read-only accessor only
-}
maxFetchTick :: Getting Int TUIState Int
maxFetchTick = to maxFetchTick'

{- | timeZone lens
custom getter to provide a read-only accessor only
-}
timeZone :: Getting TimeZone TUIState TimeZone
timeZone = to timeZone'

data TUIStorage = TUIStorage
  { stgCurrentView :: View
  , stgAnimate :: Bool
  , stgExtraInfo :: Bool
  , stgSelectedFiat :: Fiat
  , stgShowMenu :: Bool
  , stgSelectedBitcoin :: Bitcoin
  , stgBtcAmount :: Amount BTC
  , stgAssetTicker :: Ticker
  }
  deriving (Generic, Show)

instance A.ToJSON TUIStorage

instance A.FromJSON TUIStorage

type AppEventM = ReaderT AppEventEnv (EventM TUIResource TUIState)

type TUIForm e = Form TUIState e TUIResource

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI.Types where

import Brick (BrickEvent, EventM)
import Brick.BChan (BChan)
import Brick.Forms (Form)
import Brick.Types
  (
  )
import Control.Concurrent.STM (TChan)
import Control.Monad.Reader (ReaderT)
import Data.Time.LocalTime (TimeZone)
import Lens.Micro.TH (makeLenses)
import TUI.Service.Types (Amount, ApiEvent, Bitcoin (..), BlockRD, FeesRD, Fiat (..), PricesRD)

-- Resource names (needed to handle events of form elements etc.)
-- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#resource-names
data TUIResource
  = ConverterFiatField
  | ConverterBtcField
  | ConverterSatField
  deriving (Eq, Ord, Show)

newtype AppEventEnv = AppEventEnv
  { outChan :: TChan ApiEvent
  }

data ServiceEnv = ServiceEnv
  { envMempoolUrl :: String,
    envInChan :: BChan TUIEvent
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
  | FPSTick
  deriving (Show, Eq)

data ConverterData = ConverterData
  { -- | selected fiat
    _cdFiat :: Fiat,
    _cdUsd :: Amount 'USD,
    _cdCAD :: Amount 'CAD,
    _cdEUR :: Amount 'EUR,
    _cdGBP :: Amount 'GBP,
    _cdCHF :: Amount 'CHF,
    _cdJPY :: Amount 'JPY,
    _cdAUD :: Amount 'AUD,
    _cdBTC :: Amount 'BTC,
    _cdSATS :: Amount 'SATS
  }
  deriving (Eq, Show)

makeLenses ''ConverterData

data ConverterField
  = FiatField
  | BTCField
  | SatsField
  deriving (Eq, Ord, Show)

type ConverterForm = Form ConverterData TUIEvent TUIResource

data View = FeesView | PriceView | BlockView | ConverterView
  deriving (Eq)

data TUIState = TUIState
  { _timeZone :: TimeZone,
    _currentView :: View,
    _converterForm :: ConverterForm,
    _animate :: Bool,
    _tick :: Int,
    _fetchTick :: Int,
    _lastFetchTick :: Int,
    _prices :: PricesRD,
    _fees :: FeesRD,
    _block :: BlockRD,
    _selectedFiat :: Fiat,
    _selectedBitcoin :: Bitcoin,
    _stLastBrickEvent :: Maybe (BrickEvent () TUIEvent)
  }

makeLenses ''TUIState

type AppEventM = ReaderT AppEventEnv (EventM TUIResource TUIState)

type TUIForm e = Form TUIState e TUIResource

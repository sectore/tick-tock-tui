{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI.Types where

import Brick (EventM)
import Brick.Types
  (
  )
import Control.Concurrent.STM (TChan)
import Control.Monad.Reader (ReaderT)
import Lens.Micro.TH (makeLenses)
import TUI.Service.Types (ApiEvent, Currency (..), FeesRD, PricesRD)

newtype AppEventEnv = AppEventEnv
  { outChan :: TChan ApiEvent
  }

type AppEventM = ReaderT AppEventEnv (EventM () TUIState)

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
  | FPSTick
  deriving (Show, Eq)

data View = FeesView | PriceView | BlockView | ConverterView | DraftView
  deriving (Eq)

data TUIState = TUIState
  { _currentView :: View,
    _tick :: Int,
    _prices :: PricesRD,
    _fees :: FeesRD,
    _lastFetchTime :: Int,
    _selectedCurrency :: Currency
  }

makeLenses ''TUIState

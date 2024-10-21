{-# LANGUAGE TemplateHaskell #-}

module TUI.Types where

import Brick.Types
  ( BrickEvent (..),
  )
import Client.Client (FeesRD, PricesRD)
import Client.Types (Currency)
import Lens.Micro.TH (makeLenses)

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
  = Counter
  | PriceUpdated PricesRD
  | FeesUpdated FeesRD
  | FPSTick
  deriving (Show, Eq)

data TUIState = TUIState
  { _stLastBrickEvent :: Maybe (BrickEvent () TUIEvent),
    _stCounter :: Int,
    _tick :: Int,
    _price :: PricesRD,
    _fees :: FeesRD,
    _lastFetchTime :: Int,
    _selectedCurrency :: Currency
  }

makeLenses ''TUIState

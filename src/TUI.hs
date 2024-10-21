{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI where

import Brick.AttrMap
  ( attrMap,
  )
import Brick.BChan
import Brick.Main
  ( App (..),
    customMainWithDefaultVty,
    halt,
    showFirstCursor,
  )
import Brick.Types
  ( BrickEvent (..),
    EventM,
    Widget,
  )
import Brick.Widgets.Center
import Brick.Widgets.Core
  ( str,
    (<=>),
  )
import Client.Client qualified as C
import Client.Types
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async.Internal (tryAll)
import Control.Concurrent.STM (newTChanIO)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Text (pack, toUpper, unpack)
import GHC.Conc (ThreadId)
import Graphics.Vty
  ( Vty,
  )
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import TUI.Types

eighthBlocks :: (Eq a, Num a, Ord a) => a -> Char
eighthBlocks n
  | n <= 0 = ' '
  | n == 1 = '▁'
  | n == 2 = '▂'
  | n == 3 = '▃'
  | n == 4 = '▄'
  | n == 5 = '▅'
  | n == 6 = '▆'
  | n == 7 = '▇'
  | otherwise = '█'

blocks :: [Char]
blocks = [' ', '▁', '▂', '▄', '▅', '▆', '▇', '█', '▇', '▆', '▆', '▄', '▂', '▁']

spinner :: [Char]
spinner =
  [ '⠋',
    '⠙',
    '⠹',
    '⠸',
    '⠼',
    '⠴',
    '⠦',
    '⠧',
    '⠇',
    '⠏'
  ]

bar :: [String]
bar =
  [ "▒▒▒▒▒▒▒▒▒▒",
    "█▒▒▒▒▒▒▒▒▒",
    "███▒▒▒▒▒▒▒",
    "█████▒▒▒▒▒",
    "███████▒▒▒",
    "██████████",
    "▒▒▒███████",
    "▒▒▒▒▒█████",
    "▒▒▒▒▒▒▒███",
    "▒▒▒▒▒▒▒▒▒█",
    "▒▒▒▒▒▒▒▒▒▒",
    "▒▒▒▒▒▒▒▒▒█",
    "▒▒▒▒▒▒▒███",
    "▒▒▒▒▒█████",
    "▒▒▒███████",
    "██████████",
    "███████▒▒▒",
    "█████▒▒▒▒▒",
    "███▒▒▒▒▒▒▒",
    "█▒▒▒▒▒▒▒▒▒"
  ]

slogan :: [String]
slogan = ["Tick", "Tock", "Next", "Block"]

clock :: [Char]
clock = ['🕐', '🕑', '🕒', '🕓', '🕔', '🕕', '🕖', '🕗', '🕘', '🕙', '🕚', '🕛']

drawUI :: TUIState -> [Widget ()]
drawUI st = [hCenter $ vCenter a]
  where
    t = st ^. tick `div` 5
    -- tc = st ^. tick `div` 20
    tsl = st ^. tick `div` 40
    sl = mod tsl $ length slogan
    b :: Int = mod t $ length blocks
    bb :: Int = mod t $ length bar
    s :: Int = mod t $ length spinner
    c = mod tsl $ length clock
    a =
      str ("Last event: " <> show (st ^. stLastBrickEvent))
        <=> str ("Counter value is: " <> show (st ^. stCounter))
        <=> str ("tick: " <> show (st ^. tick))
        <=> str ("" <> show b)
        <=> str ("" <> [spinner !! s])
        <=> str ("" <> [blocks !! b])
        <=> str (bar !! bb)
        <=> str ([clock !! c] <> " " <> (unpack . toUpper . pack $ slogan !! sl))
        <=> str ("Price" <> show (st ^. price))
        <=> str ("Fees" <> show (st ^. fees))

wFetchData :: TChan ClientEvent -> IO ()
wFetchData outCh = STM.atomically $ STM.writeTChan outCh FetchData

appEvent :: TChan ClientEvent -> BrickEvent () TUIEvent -> EventM () TUIState ()
appEvent outCh e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent _ -> stLastBrickEvent .= Just e
    AppEvent Counter -> do
      stCounter %= (+ 1)
      stLastBrickEvent .= Just e
    AppEvent ev | ev == tickEvent -> do
      -- count `tick` by 1, but don't count it endless.
      -- Set it back to 0 if `tick` > 216000 (1h at 60 FPS)
      tick %= (`mod` 216001) . (+ 1)
      -- tick %= (+ 1)
      currentTick <- use tick
      lastFetch <- use lastFetchTime
      -- 10800 ticks = 3min seconds at 60 FPS
      when (currentTick - lastFetch >= 10800) $ do
        -- set `Loading` price
        mCurrentPrice <-
          use price <&> \case
            Success pr -> Just pr
            _ -> Nothing
        price .= Loading mCurrentPrice
        -- set `Loading` fees
        mCurrentFees <-
          use fees <&> \case
            Success pr -> Just pr
            _ -> Nothing
        fees .= Loading mCurrentFees
        -- reset last fetch time
        lastFetchTime .= currentTick
        liftIO $ wFetchData outCh
    AppEvent (PriceUpdated p) -> do
      price .= p
    AppEvent (FeesUpdated f) -> do
      fees .= f
    _ -> return ()

fetchPrices :: String -> BChan TUIEvent -> IO ()
fetchPrices url inCh = do
  -- TODO: Remove delay
  threadDelay 1_000_000
  priceResult <- tryAll $ C.fetchData url
  rd <- case priceResult of
    Left e -> return $ Failure $ "Async error: " ++ show e
    Right (Left err) -> return $ Failure $ "HTTP/Parsing error: " ++ err
    Right (Right pd) -> return $ Success pd
  writeBChan inCh (PriceUpdated rd)

fetchFees :: String -> BChan TUIEvent -> IO ()
fetchFees url inCh = do
  -- TODO: Remove delay
  threadDelay 1_000_000
  feesResult <- tryAll $ C.fetchData url
  rd <- case feesResult of
    Left e -> return $ Failure $ "Async error: " ++ show e
    Right (Left err) -> return $ Failure $ "HTTP/Parsing error: " ++ err
    Right (Right pd) -> return $ Success pd
  writeBChan inCh (FeesUpdated rd)

fetchData :: BChan TUIEvent -> IO [ThreadId]
fetchData inCh = do
  pId <- forkIO $ fetchPrices "https://mempool.space/api/v1/prices" inCh
  fId <- forkIO $ fetchFees "https://mempool.space/api/v1/fees/recommended" inCh
  pure [pId, fId]

-- Creates a Brick application by providing an `TickEvent`
-- which is sent to the Brick application by a custom defined time interval
-- TODO: Extract to a (simple) library??
customMainWithInterval ::
  (Ord n, HasTickEvent e) =>
  -- | interval in microseconds
  Int ->
  -- | Custom event channel sending into Brick app
  Maybe (BChan e) ->
  -- | Brick application
  App s e n ->
  -- | Initial application state
  s ->
  IO (s, Vty)
customMainWithInterval ms mUserChan app initialAppState = do
  inCh <- case mUserChan of
    Nothing -> liftIO $ newBChan 10
    Just uc -> pure uc

  _ <- forkIO $ forever $ do
    writeBChan inCh tickEvent
    threadDelay ms

  customMainWithDefaultVty (Just inCh) app initialAppState

run :: IO ()
run = do
  outCh <- newTChanIO
  -- \^ out channel to send messages out of TUI app
  inCh <- newBChan 10
  -- \^ in(to) channel to send messages into TUI app

  -- listen for messages outcoming from TUI app
  foreverId <- forkIO $ forever $ do
    e <- STM.atomically $ STM.readTChan outCh
    case e of
      FetchData -> do
        _ <- fetchPrices "https://mempool.space/api/v1/prices" inCh
        _ <- fetchFees "https://mempool.space/api/v1/fees/recommended" inCh
        pure ()

  -- run TUI app
  _ <- customMainWithInterval interval (Just inCh) (theApp outCh) initialState

  -- kill threads
  killThread foreverId
  where
    interval = 1_000_000 `div` 60 -- 60 FPS
    initialState :: TUIState
    initialState =
      TUIState
        { _stLastBrickEvent = Nothing,
          _stCounter = 0,
          _tick = 0,
          _price = NotAsked,
          _fees = NotAsked,
          _lastFetchTime = 0,
          _selectedCurrency = EUR
        }

    theApp :: TChan ClientEvent -> App TUIState TUIEvent ()
    theApp outCh =
      App
        { appDraw = drawUI,
          appChooseCursor = showFirstCursor,
          appHandleEvent = appEvent outCh,
          appStartEvent = do
            -- fetch all data at start
            liftIO $ wFetchData outCh
            pure (),
          appAttrMap = const $ attrMap V.defAttr []
        }

{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Exception (evaluate)
import TUI.Service.Types
import TUI.Utils
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Conversion" $ do
    describe "BTC" $ do
      it "to SATS" $ do
        toSats (Amount 0.00000010 :: Amount BTC) `shouldBe` (Amount 10 :: Amount SATS)
        toSats (Amount 1.00176761 :: Amount BTC) `shouldBe` (Amount 100176761 :: Amount SATS)
      it "to FIAT" $ do
        btcToFiat
          (Amount 0.00001000 :: Amount BTC)
          (Price 100_000 :: Price EUR)
          `shouldBe` (Amount 1 :: Amount EUR)
        -- round up
        btcToFiat
          (Amount 1.00001019 :: Amount BTC)
          (Price 100_000 :: Price EUR)
          `shouldBe` (Amount 100_001.02 :: Amount EUR)
        -- round down
        btcToFiat
          (Amount 1.00001011 :: Amount BTC)
          (Price 100_000 :: Price EUR)
          `shouldBe` (Amount 100_001.01 :: Amount EUR)
    describe "SATS" $ do
      it "to BTC" $ do
        let amount = (Amount 12345678 :: Amount SATS)
        toBtc amount `shouldBe` Amount 0.12345678
      it "to FIAT" $ do
        satsToFiat
          (Amount 1000 :: Amount SATS)
          (Price 100_000 :: Price EUR)
          `shouldBe` (Amount 1 :: Amount EUR)
        -- round up
        satsToFiat
          (Amount 1019 :: Amount SATS)
          (Price 100_000 :: Price EUR)
          `shouldBe` (Amount 1.02 :: Amount EUR)
        -- round down
        satsToFiat
          (Amount 1011 :: Amount SATS)
          (Price 100_000 :: Price EUR)
          `shouldBe` (Amount 1.01 :: Amount EUR)
    describe "EUR" $ do
      let amount = (Amount 50_000 :: Amount EUR)
      it "to BTC" $ do
        fiatToBtc amount (Price 100_000 :: Price EUR) `shouldBe` Amount 0.5
      it "to SATS" $
        fiatToSats amount (Price 100_000 :: Price EUR) `shouldBe` Amount 50_000_000
    describe "USD" $ do
      it "to BTC" $ do
        fiatToBtc (Amount 50_000 :: Amount USD) (Price 200_000 :: Price USD) `shouldBe` (Amount 0.25 :: Amount BTC)
        fiatToBtc (Amount 50_000.01 :: Amount USD) (Price 200_000 :: Price USD) `shouldBe` (Amount 0.25000005 :: Amount BTC)
        fiatToBtc (Amount 50_000.09 :: Amount USD) (Price 200_000 :: Price USD) `shouldBe` (Amount 0.25000045 :: Amount BTC)
      it "to SATS" $ do
        fiatToSats (Amount 50_000 :: Amount USD) (Price 200_000 :: Price USD) `shouldBe` Amount 25_000_000
        fiatToSats (Amount 50_000.01 :: Amount USD) (Price 200_000 :: Price USD) `shouldBe` (Amount 25000005 :: Amount SATS)
        fiatToSats (Amount 50_000.09 :: Amount USD) (Price 200_000 :: Price USD) `shouldBe` (Amount 25000045 :: Amount SATS)

  describe "Show Amount a / ShowAmount" $ do
    it "USD" $
      show (Amount 50_000 :: Amount USD) `shouldBe` "USD 50000"
    it "EUR" $ do
      show (Amount 10_000 :: Amount EUR) `shouldBe` "EUR 10000"
      -- check some rounded cases
      show (Amount 10_000.999 :: Amount EUR) `shouldBe` "EUR 10001"
      show (Amount 10_000.99 :: Amount EUR) `shouldBe` "EUR 10000.99"
      show (Amount 10_000.009 :: Amount EUR) `shouldBe` "EUR 10000.01"
      show (Amount 10_000.001 :: Amount EUR) `shouldBe` "EUR 10000"
    it "AUD" $
      show (Amount 75_000 :: Amount AUD) `shouldBe` "AUD 75000"
    it "CAD" $
      show (Amount 25_000 :: Amount CAD) `shouldBe` "CAD 25000"
    it "JPY" $
      show (Amount 100_000 :: Amount JPY) `shouldBe` "JPY 100000"
    it "CHF" $
      show (Amount 15_000.01 :: Amount CHF) `shouldBe` "CHF 15000.01"
    it "GBP" $
      show (Amount 30_000 :: Amount GBP) `shouldBe` "GBP 30000"
    it "BTC" $ do
      show (Amount 1.23 :: Amount BTC) `shouldBe` "BTC 1.230 000 00"
      show (Amount 1.00176761 :: Amount BTC) `shouldBe` "BTC 1.001 767 61"
    it "sats" $
      show (Amount 123 :: Amount SATS) `shouldBe` "123 sats"

  describe "Read Amount a" $ do
    describe "Fiat" $ do
      it "USD" $
        do
          read "USD 50000.00" `shouldBe` (Amount 50000 :: Amount USD)
          read "USD 50000.01" `shouldBe` (Amount 50000.01 :: Amount USD)
          evaluate (read "500" :: Amount USD)
          `shouldThrow` errorCall "Prelude.read: no parse"

      it "EUR" $
        do
          read "EUR 10000.00" `shouldBe` (Amount 10000 :: Amount EUR)
          read "EUR 10000.50" `shouldBe` (Amount 10000.50 :: Amount EUR)
          evaluate (read "1000" :: Amount EUR)
          `shouldThrow` errorCall "Prelude.read: no parse"

      it "GBP" $
        do
          read "GBP 30000.00" `shouldBe` (Amount 30000 :: Amount GBP)
          read "GBP 30000.75" `shouldBe` (Amount 30000.75 :: Amount GBP)
          evaluate (read "3000" :: Amount GBP)
          `shouldThrow` errorCall "Prelude.read: no parse"

      it "CAD" $
        do
          read "CAD 25000.00" `shouldBe` (Amount 25000 :: Amount CAD)
          read "CAD 25000.25" `shouldBe` (Amount 25000.25 :: Amount CAD)
          evaluate (read "2500" :: Amount CAD)
          `shouldThrow` errorCall "Prelude.read: no parse"

      it "CHF" $
        do
          read "CHF 15000.00" `shouldBe` (Amount 15000 :: Amount CHF)
          read "CHF 15000.15" `shouldBe` (Amount 15000.15 :: Amount CHF)
          evaluate (read "1500" :: Amount CHF)
          `shouldThrow` errorCall "Prelude.read: no parse"

      it "AUD" $
        do
          read "AUD 75000.00" `shouldBe` (Amount 75000 :: Amount AUD)
          read "AUD 75000.75" `shouldBe` (Amount 75000.75 :: Amount AUD)
          evaluate (read "7500" :: Amount AUD)
          `shouldThrow` errorCall "Prelude.read: no parse"

      it "JPY" $
        do
          read "JPY 100000.00" `shouldBe` (Amount 100000 :: Amount JPY)
          read "JPY 100000.50" `shouldBe` (Amount 100000.50 :: Amount JPY)
          evaluate (read "10000" :: Amount JPY)
          `shouldThrow` errorCall "Prelude.read: no parse"

      describe "Bitcoin" $ do
        it "BTC" $
          do
            read "BTC 1.23456789" `shouldBe` (Amount 1.23456789 :: Amount BTC)
            read "BTC 0.00000001" `shouldBe` (Amount 0.00000001 :: Amount BTC)
            -- missing empty space but still readable
            read "BTC0.00000002" `shouldBe` (Amount 0.00000002 :: Amount BTC)
            -- default format
            read "BTC 1.001 000 01" `shouldBe` (Amount 1.00100001 :: Amount BTC)
            -- missing BTC
            evaluate (read "1.23456789" :: Amount BTC)
            `shouldThrow` errorCall "Prelude.read: no parse"

        it "SATS" $
          do
            read "123456 sats" `shouldBe` (Amount 123456 :: Amount SATS)
            read "1 sats" `shouldBe` (Amount 1 :: Amount SATS)
            evaluate (read "123456" :: Amount SATS)
            `shouldThrow` errorCall "Prelude.read: no parse"

{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Exception (evaluate)
import TUI.Service.Types
import TUI.Utils
import Test.Hspec
import Text.Printf (printf)

main :: IO ()
main = hspec $ do
  describe "Conversion" $ do
    describe "BTC" $ do
      it "to SATS" $
        toSats (Amount 0.00000010 :: Amount BTC) `shouldBe` Amount 10
    describe "SATS" $ do
      it "to BTC" $ do
        let amount = (Amount 12345678 :: Amount SATS)
        printf $ show amount
        toBtc amount `shouldBe` Amount 0.12345678
    describe "EUR" $ do
      let amount = (Amount 50_000 :: Amount EUR)
      it "to BTC" $ do
        printf $ show amount
        fiatToBtc amount (Price 100_000) `shouldBe` Amount 0.5
      it "to SATS" $
        fiatToSats amount (Price 100_000) `shouldBe` Amount 50_000_000
    describe "USD" $ do
      let amount = (Amount 50_000 :: Amount USD)
      it "to BTC" $
        fiatToBtc amount (Price 200_000) `shouldBe` Amount 0.25
      it "to SATS" $
        fiatToSats amount (Price 200_000) `shouldBe` Amount 25_000_000

    describe "Show Amount a / ShowAmount" $ do
      it "USD" $
        show (Amount 50_000 :: Amount USD) `shouldBe` "USD 50000.00"
      it "EUR" $
        show (Amount 10_000 :: Amount EUR) `shouldBe` "EUR 10000.00"
      it "AUD" $
        show (Amount 75_000 :: Amount AUD) `shouldBe` "AUD 75000.00"
      it "CAD" $
        show (Amount 25_000 :: Amount CAD) `shouldBe` "CAD 25000.00"
      it "JPY" $
        show (Amount 100_000 :: Amount JPY) `shouldBe` "JPY 100000.00"
      it "CHF" $
        show (Amount 15_000 :: Amount CHF) `shouldBe` "CHF 15000.00"
      it "GBP" $
        show (Amount 30_000 :: Amount GBP) `shouldBe` "GBP 30000.00"
      it "BTC" $
        show (Amount 1.23 :: Amount BTC) `shouldBe` "BTC 1.23000000"
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
              evaluate (read "1.23456789" :: Amount BTC)
              `shouldThrow` errorCall "Prelude.read: no parse"

          it "SATS" $
            do
              read "123456 sats" `shouldBe` (Amount 123456 :: Amount SATS)
              read "1 sats" `shouldBe` (Amount 1 :: Amount SATS)
              evaluate (read "123456" :: Amount SATS)
              `shouldThrow` errorCall "Prelude.read: no parse"

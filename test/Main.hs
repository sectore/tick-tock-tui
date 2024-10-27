{-# LANGUAGE DataKinds #-}

module Main (main) where

import TUI.Service.Types
import Test.Hspec
import Text.Printf (printf)

main :: IO ()
main = hspec $ do
  describe "Conversion" $ do
    describe "BTC" $ do
      it "to BTC" $ do
        let amount = (Amount 0.12345678 :: Amount BTC)
        printf $ show amount
        toBtc amount (Price 1) `shouldBe` Amount 0.12345678
      it "to SATS" $
        toSats (Amount 0.00000010 :: Amount BTC) (Price 1) `shouldBe` Amount 10
    describe "SATS" $ do
      it "to BTC" $ do
        let amount = (Amount 12345678 :: Amount SATS)
        printf $ show amount
        toBtc amount (Price 1) `shouldBe` Amount 0.12345678
      it "to SATS" $
        toSats (Amount 0.00000010 :: Amount SATS) (Price 1) `shouldBe` Amount 0.00000010
    describe "EUR" $ do
      let amount = (Amount 50_000 :: Amount EUR)
      it "to BTC" $ do
        printf $ show amount
        toBtc amount (Price 100_000) `shouldBe` Amount 0.5
      it "to SATS" $
        toSats amount (Price 100_000) `shouldBe` Amount 50_000_000
    describe "USD" $ do
      let amount = (Amount 50_000 :: Amount USD)
      it "to BTC" $ do
        printf $ show amount
        toBtc amount (Price 200_000) `shouldBe` Amount 0.25
      it "to SATS" $
        toSats amount (Price 200_000) `shouldBe` Amount 25_000_000

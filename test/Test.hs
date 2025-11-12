module Main where

import Control.Exception (evaluate)
import Test.Hspec
  ( SpecWith (..),
    anyException,
    describe,
    hspec,
    it,
    shouldBe,
    shouldThrow,
  )
import Test.JS.Parse (jsParseSpec)

main :: IO ()
main = hspec $ do
  jsParseSpec

  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
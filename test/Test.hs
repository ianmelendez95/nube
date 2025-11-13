module Main where

import Control.Exception (evaluate)
import JS.TestParse (jsParseSpec)
import JS.TestTranspile (jsTranspileSpec)
import Test.Hspec
  ( SpecWith (..),
    anyException,
    describe,
    hspec,
    it,
    shouldBe,
    shouldThrow,
  )

main :: IO ()
main = hspec $ do
  jsParseSpec

  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
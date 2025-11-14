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
  jsTranspileSpec
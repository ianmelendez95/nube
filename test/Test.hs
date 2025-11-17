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
import Test.Nube.Cont (jsCompileContSpec)
import Test.Nube.Parse (jsParseSpec)
import Test.Nube.Rename (jsTranspileSpec)

main :: IO ()
main = hspec $ do
  jsParseSpec
  jsTranspileSpec
  jsCompileContSpec
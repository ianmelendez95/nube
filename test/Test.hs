module Main where

import Control.Exception (evaluate)
import Test.Compile.Cont (jsCompileContSpec)
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
import Test.JS.Transpile (jsTranspileSpec)

main :: IO ()
main = hspec $ do
  jsParseSpec
  jsTranspileSpec
  jsCompileContSpec
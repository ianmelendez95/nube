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
import Test.Nube.Parse (jsParseSpec)
import Test.Nube.Rename (jsTranspileSpec)
import Test.Nube.Syntax (testSyntax)
import Test.Nube.St (testSt)

main :: IO ()
main = hspec $ do
  jsParseSpec
  jsTranspileSpec
  testSyntax
  testSt

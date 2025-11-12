{-# LANGUAGE OverloadedStrings #-}

module Test.JS.Parse
  ( jsParseSpec,
  )
where

import Data.Text
  ( Text,
    pack
  )
import JS.Parse
  ( Parser (..),
    identifier,
    dotMember
  )
import Test.Hspec
  ( SpecWith (..),
    describe,
    it,
    shouldBe,
  )
import Text.Megaparsec
  ( errorBundlePretty,
    runParser,
  )

jsParseSpec :: SpecWith ()
jsParseSpec = do 
  testIdentifier

testIdentifier :: SpecWith ()
testIdentifier =
  describe "identifier" $ do
    it "returns the identifier" $ do
      id <- testParser identifier "hello"
      id `shouldBe` "hello"

testDotMember =
  describe "dotMember" $ do
    it "returns the property name" $ do
      prop <- testParser dotMember ".someProp"
      prop `shouldBe` "someProp"

testParser :: Parser a -> Text -> IO a
testParser parser content = do
  let result = either (error . errorBundlePretty) id $ runParser parser "test.js" content
  pure result

{-# LANGUAGE OverloadedStrings #-}

module Test.JS.Parse
  ( jsParseSpec,
  )
where

import Data.Text
  ( Text,
    pack
  )

import JS.Syntax
  (Expr (..)
  )
import JS.Parse
  ( Parser (..),
    identifier,
    dotMember,
    stringLitExpr,
    member
  )
import Test.Hspec
  ( SpecWith (..),
    describe,
    xdescribe,
    it,
    shouldBe,
  )
import Text.Megaparsec
  ( errorBundlePretty,
    runParser,
  )

jsParseSpec = do 
  describe "member" $ do 
    it "parses dot member" $ do 
      res <- testParser member "someObj.someProp"
      res `shouldBe` EDotMember "someObj" "someProp"

    it "parses bracket member" $ do 
      res <- testParser member "someObj[\"someProp\"]"
      res `shouldBe` EBracketMember "someObj" (EStringLit "someProp")

  describe "identifier" $ do
    it "returns the identifier" $ do
      id <- testParser identifier "hello"
      id `shouldBe` "hello"

  describe "dotMember" $ do
    it "returns the property name" $ do
      prop <- testParser dotMember ".someProp"
      prop `shouldBe` "someProp"

  describe "stringLitExpr" $ do
    it "parses double quoted" $ do
      slit <- testParser stringLitExpr "\"hello world!\""
      slit `shouldBe` EStringLit "hello world!"

    it "parses single quoted" $ do
      slit <- testParser stringLitExpr "'hello world!'"
      slit `shouldBe` EStringLit "hello world!"

testParser :: Parser a -> Text -> IO a
testParser parser content = do
  let result = either (error . errorBundlePretty) id $ runParser parser "test.js" content
  pure result

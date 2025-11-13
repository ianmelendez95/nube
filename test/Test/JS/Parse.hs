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
  , EAccess (..)
  )
import JS.Parse
  ( Parser (..),
    identifier,
    dotMember,
    bracketMember,
    stringLitExpr,
    expr
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
  describe "expr" $ do 
    it "parses string literal" $ do 
      res <- testParser expr "\"hello world!\""
      res `shouldBe` EStringLit "hello world!"
    
    it "parses dot member access" $ do 
      res <- testParser expr "string.split"
      res `shouldBe` EMember (EVar "string") (EDotAccess "split")
    
    it "parses bracket member access" $ do
      res <- testParser expr "string['split']"
      res `shouldBe` EMember (EVar "string") (EBracketAccess (EStringLit "split")) 

  describe "identifier" $ do
    it "returns the identifier" $ do
      id <- testParser identifier "hello"
      id `shouldBe` "hello"

  describe "dotMember" $ do
    it "returns the property name" $ do
      prop <- testParser dotMember ".someProp"
      prop `shouldBe` EDotAccess "someProp"
  
  describe "bracketMember" $ do 
    it "parses simple bracket" $ do 
      prop <- testParser bracketMember "['someProp']"
      prop `shouldBe` EBracketAccess (EStringLit "someProp")

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

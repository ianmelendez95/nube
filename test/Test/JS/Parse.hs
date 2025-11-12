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
jsParseSpec = testIdentifier

testIdentifier :: SpecWith ()
testIdentifier =
  describe "identifier" $ do
    it "returns the identifier" $ do
      id <- testParser identifier (pack "hello")
      id `shouldBe` (pack "hello")

testParser :: Parser a -> Text -> IO a
testParser parser content = do
  let result = either (error . errorBundlePretty) id $ runParser parser "test.js" content
  pure result

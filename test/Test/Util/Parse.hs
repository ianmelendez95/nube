module Test.Util.Parse (testParser, runParser) where

import Data.Text (Text)
import JS.Parse
  ( Parser,
  )
import Text.Megaparsec qualified as MP
  ( errorBundlePretty,
    runParser,
  )

testParser :: Parser a -> Text -> IO a
testParser parser = pure . runParser parser

runParser :: Parser a -> Text -> a
runParser parser content = either (error . MP.errorBundlePretty) id $ MP.runParser parser "test.js" content
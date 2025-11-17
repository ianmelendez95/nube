module Test.Util.Parse (testParser, runParser) where

import Data.Text (Text)
import Nube.Parse qualified as NP
  ( PContext (..),
    Parser,
    runParser,
  )
import Text.Megaparsec qualified as MP
  ( errorBundlePretty,
  )

testParser :: NP.Parser a -> Text -> IO a
testParser parser = pure . runParser parser

runParser :: NP.Parser a -> Text -> a
runParser parser content =
  let parse_result = NP.runParser (NP.PContext []) parser "test.js" content
   in fst $ either (error . MP.errorBundlePretty) id parse_result
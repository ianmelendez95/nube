module Test.Util.Parse (testParser, runParser, runParser') where

import Data.Text (Text)
import Nube.Compiler (CContext (..))
import Nube.Parse qualified as NP
  ( Parser,
    runParser,
  )
import Text.Megaparsec qualified as MP
  ( errorBundlePretty,
  )

testParser :: NP.Parser a -> Text -> IO a
testParser parser = pure . runParser parser

runParser :: NP.Parser a -> Text -> a
runParser parser = fst . runParser' parser

runParser' :: NP.Parser a -> Text -> (a, CContext)
runParser' parser content =
  let parse_result = NP.runParser (CContext []) parser "test.js" content
   in either (error . MP.errorBundlePretty) id parse_result
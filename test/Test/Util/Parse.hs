module Test.Util.Parse
  ( testParser,
    testParser',
    runParser,
    runParser',
    tryParserWithContext,
  )
where

import Data.Text (Text)
import Nube.Context (NContext (..))
import Nube.Parse qualified as NP
  ( Parser,
    runParser,
  )
import Nube.Parser (PErrorBundle)
import Text.Megaparsec qualified as MP
  ( errorBundlePretty,
  )

testParser :: NP.Parser a -> Text -> IO a
testParser parser content = fst <$> testParser' parser content

testParser' :: forall a. NP.Parser a -> Text -> IO (a, NContext)
testParser' parser content = do
  testParserWithContext parser (NContext []) content

testParserWithContext :: forall a. NP.Parser a -> NContext -> Text -> IO (a, NContext)
testParserWithContext parser context content = do
  let parse_result :: Either PErrorBundle (a, NContext)
      parse_result = tryParserWithContext parser context content
  either (fail . MP.errorBundlePretty) pure parse_result

runParser :: NP.Parser a -> Text -> a
runParser parser = fst . runParser' parser

runParser' :: NP.Parser a -> Text -> (a, NContext)
runParser' parser content =
  let parse_result = tryParserWithContext parser (NContext []) content
   in either (error . MP.errorBundlePretty) id parse_result

tryParserWithContext :: forall a. NP.Parser a -> NContext -> Text -> Either PErrorBundle (a, NContext)
tryParserWithContext parser context = NP.runParser context parser "test.js"

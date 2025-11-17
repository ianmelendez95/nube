module Nube.Parser
  ( Parser,
    PState,
    PErrorBundle,
    runParser,
  )
where

import Control.Monad.State (State, modify, runState)
import Data.Text qualified as T
import Data.Void (Void)
import Nube.Context (NContext (..))
import Text.Megaparsec
  ( ParsecT,
    runParserT,
  )
import Text.Megaparsec.Error (ParseErrorBundle)

-- ParsecT e=Void s=T.Text m=PState a
type Parser = ParsecT Void T.Text PState

type PState = State NContext

type PErrorBundle = ParseErrorBundle T.Text Void

runParser :: forall a. NContext -> Parser a -> FilePath -> T.Text -> Either PErrorBundle (a, NContext)
runParser context p path = joinEither . (`runState` context) . runParserT p path
  where
    joinEither :: (Either PErrorBundle a, NContext) -> Either PErrorBundle (a, NContext)
    joinEither (result_a, ctx) = (,ctx) <$> result_a
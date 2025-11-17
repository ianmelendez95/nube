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
import Nube.Compiler (CContext (..), ctxAddFn)
import Text.Megaparsec
  ( ParsecT,
    runParserT,
  )
import Text.Megaparsec.Error (ParseErrorBundle)

-- ParsecT e=Void s=T.Text m=PState a
type Parser = ParsecT Void T.Text PState

type PState = State CContext

type PErrorBundle = ParseErrorBundle T.Text Void

runParser :: forall a. CContext -> Parser a -> FilePath -> T.Text -> Either PErrorBundle (a, CContext)
runParser context p path = joinEither . (`runState` context) . runParserT p path
  where
    joinEither :: (Either PErrorBundle a, CContext) -> Either PErrorBundle (a, CContext)
    joinEither (result_a, ctx) = (,ctx) <$> result_a
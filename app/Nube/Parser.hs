module Nube.Parser
  ( Parser,
    PState,
    PContext (..),
    PErrorBundle,
    runParser,
    addFName,
  )
where

import Control.Monad.State (State, modify, runState)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
  ( ParsecT,
    runParserT,
  )
import Text.Megaparsec.Error (ParseErrorBundle)

-- ParsecT e=Void s=T.Text m=PState a
type Parser = ParsecT Void T.Text PState

type PState = State PContext

newtype PContext = PContext
  { cUserFns :: [T.Text]
  }
  deriving (Show, Eq)

type PErrorBundle = ParseErrorBundle T.Text Void

runParser :: forall a. PContext -> Parser a -> FilePath -> T.Text -> Either PErrorBundle (a, PContext)
runParser context p path = joinEither . (`runState` context) . runParserT p path
  where
    joinEither :: (Either PErrorBundle a, PContext) -> Either PErrorBundle (a, PContext)
    joinEither (result_a, ctx) = (,ctx) <$> result_a

addFName :: T.Text -> Parser ()
addFName = modify . addInCtx
  where
    addInCtx :: T.Text -> PContext -> PContext
    addInCtx fname (PContext fnames) = PContext (fname : fnames)
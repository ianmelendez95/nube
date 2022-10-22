{-# LANGUAGE OverloadedStrings #-}

module JS.Parse where 

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Parser = Parsec Void T.Text

parseJsFile :: FilePath -> IO [T.Text]
parseJsFile path = do 
  content <- TIO.readFile path
  let result = either (error . errorBundlePretty) id $ runParser jsFunctions path content
  pure result

jsFunctions :: Parser [T.Text]
jsFunctions = many (lexeme asyncFunction)

asyncFunction :: Parser T.Text
asyncFunction = do 
  _ <- symbol "async" >> symbol "function"
  ident  <- lexeme identifier
  params <- lexeme parameters
  bdy    <- body
  pure $ ident <> params <> bdy
  where 
    -- Parameters of the function
    parameters :: Parser T.Text
    parameters = do 
      opar   <- symbol "(" 
      params <- takeWhileP (Just "not paren") (\c -> c /= '(' && c /= ')')
      cpar   <- symbol ")"
      pure $ opar <> params <> cpar

    body :: Parser T.Text
    body = do 
      ocurl <- symbol "{"
      bdy   <- bodyContent
      ccurl <- symbol "}"
      pure $ ocurl <> bdy <> ccurl

    bodyContent :: Parser T.Text
    bodyContent = do 
      pre_brace <- takeWhileP (Just "not curly brace") (\c -> c /= '{' && c /= '}')
      next      <- lookAhead anySingle
      case next of 
        '{' -> do 
          inner_bdy    <- body
          rest_content <- bodyContent
          pure $ pre_brace <> inner_bdy <> rest_content
        '}' -> pure pre_brace
        _ -> fail $ "Expecting to stop at a curly brace, got: " <> [next]

identifier :: Parser T.Text
identifier = do 
  T.cons <$> letterChar <*> takeWhileP (Just "identifier char") (\c -> isAlphaNum c || c == '_' || c == '-')

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space'

space' :: Parser ()
space' = L.space space1 empty empty

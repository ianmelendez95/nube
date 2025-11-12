{-# LANGUAGE OverloadedStrings #-}

module JS.Parse where 

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath

import qualified JS.Syntax as S

type Parser = Parsec Void T.Text

parseJsFile :: FilePath -> IO S.Script
parseJsFile path = do 
  content <- TIO.readFile path
  let result = either (error . errorBundlePretty) id $ runParser jsFunctions path content
  pure $ S.Script (T.pack $ takeBaseName path) result

jsFunctions :: Parser [S.Fn]
jsFunctions = many asyncFunction

asyncFunction :: Parser S.Fn
asyncFunction = do 
  _ <- symbol "async" >> symbol "function"
  S.Fn <$> lexeme identifier
        <*> lexeme parameters
        <*> body
  where 
    -- Parameters of the function
    parameters :: Parser T.Text
    parameters = between' (symbol' "(") 
                          (symbol' ")")
                          (takeWhileP (Just "not paren") (\c -> c /= '(' && c /= ')'))

    body :: Parser T.Text
    body = between' (symbol' "{") (symbol "}") bodyContent

    innerBody :: Parser T.Text
    innerBody = between' (symbol' "{") (symbol' "}") bodyContent

    bodyContent :: Parser T.Text
    bodyContent = do 
      pre_brace <- takeWhileP (Just "not curly brace") (\c -> c /= '{' && c /= '}')
      next      <- lookAhead anySingle
      case next of 
        '{' -> do 
          inner_bdy    <- innerBody
          rest_content <- bodyContent
          pure $ pre_brace <> inner_bdy <> rest_content
        '}' -> pure pre_brace
        _ -> fail $ "Expecting to stop at a curly brace, got: " <> [next]

identifier :: Parser T.Text
identifier = do 
  T.cons <$> letterChar <*> takeWhileP (Just "identifier char") (\c -> isAlphaNum c || c == '_' || c == '-')

between' :: Parser T.Text -> Parser T.Text -> Parser T.Text -> Parser T.Text
between' bra cket p = T.concat <$> sequence [bra, p, cket]

symbol' :: T.Text -> Parser T.Text
symbol' sym = lexeme' (string sym)

symbol :: T.Text -> Parser T.Text
symbol sym = lexeme (string sym)

lexeme :: Parser a -> Parser a
lexeme p = p <* space'

lexeme' :: Parser T.Text -> Parser T.Text
lexeme' p = (<>) <$> p <*> space'

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 mempty mempty

space' :: Parser T.Text
space' = takeWhileP (Just "whitespace") isSpace

space1' :: Parser T.Text
space1' = takeWhile1P (Just "whitespace") isSpace 

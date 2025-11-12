{-# LANGUAGE OverloadedStrings #-}

module JS.Parse
  ( Parser(..),
    stringLitExpr,
    parseJsFile,
    identifier,
    dotMember,
  )
where

import Control.Monad.Combinators (between)
import Data.Char (isAlphaNum, isSpace)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import JS.Syntax qualified as S
import System.FilePath (takeBaseName)
import Text.Megaparsec
  ( MonadParsec (lookAhead, takeWhile1P, takeWhileP),
    Parsec,
    anySingle,
    between,
    errorBundlePretty,
    many,
    manyTill,
    runParser,
  )
import Text.Megaparsec.Char (letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L (charLiteral, lexeme, space, symbol)

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
  S.Fn
    <$> lexeme identifier
    <*> lexeme parameters
    <*> undefined
  where
    -- Parameters of the function
    parameters :: Parser T.Text
    parameters =
      between'
        (symbol' "(")
        (symbol' ")")
        (takeWhileP (Just "not paren") (\c -> c /= '(' && c /= ')'))

    _body :: Parser T.Text
    _body = between' (symbol' "{") (symbol "}") _bodyContent

    _innerBody :: Parser T.Text
    _innerBody = between' (symbol' "{") (symbol' "}") _bodyContent

    _bodyContent :: Parser T.Text
    _bodyContent = do
      pre_brace <- takeWhileP (Just "not curly brace") (\c -> c /= '{' && c /= '}')
      next <- lookAhead anySingle
      case next of
        '{' -> do
          inner_bdy <- _innerBody
          rest_content <- _bodyContent
          pure $ pre_brace <> inner_bdy <> rest_content
        '}' -> pure pre_brace
        _ -> fail $ "Expecting to stop at a curly brace, got: " <> [next]

statement :: Parser S.Stmt
statement = undefined

const_assign :: Parser S.Stmt
const_assign = do
  _ <- symbol "const"
  var_name <- identifier
  _ <- symbol "="
  S.SAssign var_name <$> expr

expr :: Parser S.Expr
expr = stringLitExpr

stringLitExpr :: Parser S.Expr
stringLitExpr = S.EStringLit <$> stringLiteral

member :: Parser S.MemberExpr
member = undefined

dotMember :: Parser T.Text
dotMember = do
  _ <- symbol "."
  identifier

bracketMember :: Parser S.Expr
bracketMember = between (symbol "[") (symbol "]") expr

identifier :: Parser T.Text
identifier = do
  T.cons <$> letterChar <*> takeWhileP (Just "identifier char") (\c -> isAlphaNum c || c == '_' || c == '-')

stringLiteral :: Parser T.Text
stringLiteral = T.pack <$> (symbol "\"" >> manyTill L.charLiteral (symbol "\""))

between' :: Parser T.Text -> Parser T.Text -> Parser T.Text -> Parser T.Text
between' bra cket p = T.concat <$> sequence [bra, p, cket]

symbol' :: T.Text -> Parser T.Text
symbol' sym = lexeme' (string sym)

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

lexeme' :: Parser T.Text -> Parser T.Text
lexeme' p = (<>) <$> p <*> space'

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 mempty mempty

space' :: Parser T.Text
space' = takeWhileP (Just "whitespace") isSpace

space1' :: Parser T.Text
space1' = takeWhile1P (Just "whitespace") isSpace

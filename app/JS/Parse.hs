{-# LANGUAGE OverloadedStrings #-}

module JS.Parse
  ( Parser,
    stringLitExpr,
    parseJsFile,
    identifier,
    dotMember,
    bracketMember,
    expr
  )
where

import Data.Char (isAlphaNum, isSpace)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import JS.Syntax qualified as S
import System.FilePath (takeBaseName)
import Text.Megaparsec
  ( MonadParsec (lookAhead, takeWhile1P, takeWhileP, try),
    Parsec,
    choice,
    optional,
    option,
    anySingle,
    between,
    (<|>),
    errorBundlePretty,
    many,
    manyTill,
    runParser,
    parseMaybe,
    sepBy
  )
import Text.Megaparsec.Char (letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L (charLiteral, lexeme, space, symbol, skipLineComment, skipBlockComment)

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
expr = do
  term <- exprTerm
  mmem_or_call <- optional $ choice
    [ S.EMember term <$> memberAccess
    , S.ECall term <$> callParens 
    ]
  case mmem_or_call of 
    Nothing -> pure term
    Just mem_or_call -> do 
      mmem_or_call' <- optional $ choice
        [ S.EMember mem_or_call <$> memberAccess
        , S.ECall mem_or_call <$> callParens ]
      case mmem_or_call' of 
        Nothing -> pure mem_or_call
        Just mem_or_call' -> pure mem_or_call' 


exprTerm :: Parser S.Expr
exprTerm = try varExpr <|> stringLitExpr

callExpr :: Parser S.Expr
callExpr = S.ECall <$> expr <*> callParens 

callParens :: Parser [S.Expr]
callParens = between (symbol "(") (symbol ")") (sepBy arg (symbol ","))
  where 
    arg = S.EVar <$> identifier

varExpr :: Parser S.Expr
varExpr = S.EVar <$> identifier

stringLitExpr :: Parser S.Expr
stringLitExpr = S.EStringLit <$> stringLiteral

memberAccess :: Parser S.EAccess
memberAccess = try dotMember <|> bracketMember

dotMember :: Parser S.EAccess
dotMember = do
  _ <- symbol "."
  S.EDotAccess <$> identifier

bracketMember :: Parser S.EAccess
bracketMember = S.EBracketAccess <$> between (symbol "[") (symbol "]") expr

identifier :: Parser T.Text
identifier = do
  T.cons <$> letterChar <*> takeWhileP (Just "identifier char") (\c -> isAlphaNum c || c == '_' || c == '-')

stringLiteral :: Parser T.Text
stringLiteral = stringLiteral' (symbol "\"") <|> stringLiteral' (symbol "'")

stringLiteral' :: Parser a -> Parser T.Text
stringLiteral' quotes = T.pack <$> (quotes >> manyTill L.charLiteral quotes)

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
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

space' :: Parser T.Text
space' = takeWhileP (Just "whitespace") isSpace

space1' :: Parser T.Text
space1' = takeWhile1P (Just "whitespace") isSpace

module Nube.Parse
  ( Parser,
    PContext (..),
    stringLitExpr,
    parseJsFile,
    identifier,
    dotMember,
    bracketMember,
    expr,
    statement,
    function,
    runParser,
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State (State (..), runState)
import Data.Char (isAlphaNum, isSpace)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Nube.Syntax qualified as S
import System.FilePath (takeBaseName)
import Text.Megaparsec
  ( MonadParsec (lookAhead, takeWhile1P, takeWhileP, try),
    ParsecT,
    anySingle,
    between,
    choice,
    errorBundlePretty,
    many,
    manyTill,
    optional,
    runParserT,
    sepBy,
    (<|>),
  )
import Text.Megaparsec.Char (letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L
  ( charLiteral,
    decimal,
    lexeme,
    skipBlockComment,
    skipLineComment,
    space,
    symbol,
  )
import Text.Megaparsec.Error (ParseErrorBundle)

-- import Text.Megaparsec.Debug (dbg)

-- ParsecT e=Void s=T.Text m=PState a
type Parser = ParsecT Void T.Text PState

type PState = State PContext

newtype PContext = PContext
  { cUserFns :: [T.Text]
  }

type PErrorBundle = ParseErrorBundle T.Text Void

parseJsFile :: FilePath -> IO (S.Script, PContext)
parseJsFile path = parseJsContent path <$> TIO.readFile path

parseJsContent :: FilePath -> T.Text -> (S.Script, PContext)
parseJsContent path content =
  let run_res = runParser (PContext []) jsFunctions path content
      (result, ctx) = either (error . errorBundlePretty) id run_res
   in (S.Script (T.pack $ takeBaseName path) result, ctx)

runParser :: forall a. PContext -> Parser a -> FilePath -> T.Text -> Either PErrorBundle (a, PContext)
runParser context p path = joinEither . (`runState` context) . runParserT p path
  where
    joinEither :: (Either PErrorBundle a, PContext) -> Either PErrorBundle (a, PContext)
    joinEither (result_a, ctx) = (,ctx) <$> result_a

jsFunctions :: Parser [S.Fn]
jsFunctions = many asyncFunction

asyncFunction :: Parser S.Fn
asyncFunction = do
  _ <- symbol "async" >> symbol "function"
  S.Fn
    <$> lexeme identifier
    <*> undefined
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

function :: Parser S.Fn
function = do
  _ <- symbol "function"
  S.Fn <$> identifier <*> fn_parameters <*> fn_body
  where
    fn_parameters :: Parser [T.Text]
    fn_parameters = between (symbol "(") (symbol ")") $ sepBy identifier (symbol ",")

    fn_body :: Parser [S.Stmt]
    fn_body = between (symbol "{") (symbol "}") $ many statement

statement :: Parser S.Stmt
statement = (try const_assign <|> return_stmt) <* symbol ";"
  where
    const_assign :: Parser S.Stmt
    const_assign = do
      _ <- symbol "const"
      var_name <- lexeme identifier
      _ <- symbol "="
      S.SConst var_name <$> expr

    return_stmt :: Parser S.Stmt
    return_stmt = do
      _ <- symbol "return"
      S.SReturn <$> expr

expr :: Parser S.Expr
expr = makeExprParser (lexeme term) exprTable
  where
    term = do
      t <- termTerm
      chain_access t

    -- TODO solve this conundrum, eliminate the recursion!
    chain_access :: S.Expr -> Parser S.Expr
    chain_access last_term =
      maybe_mem_or_call last_term >>= maybe (pure last_term) chain_access

    maybe_mem_or_call :: S.Expr -> Parser (Maybe S.Expr)
    maybe_mem_or_call last_term =
      optional $
        choice
          [ S.EMember last_term <$> try memberAccess,
            S.ECall last_term <$> try callParens
          ]

    termTerm =
      choice
        [ try varExpr,
          stringLitExpr,
          S.ENumberLit <$> L.decimal
        ]

exprTable :: [[Operator Parser S.Expr]]
exprTable =
  [ [binary "+" (S.EInfix S.IPlus)]
  ]
  where
    binary :: T.Text -> (S.Expr -> S.Expr -> S.Expr) -> Operator Parser S.Expr
    binary name f = InfixL (f <$ symbol name)

callParens :: Parser [S.Expr]
callParens = between (symbol "(") (symbol ")") (sepBy expr (symbol ","))

varExpr :: Parser S.Expr
varExpr = S.EVar <$> identifier

stringLitExpr :: Parser S.Expr
stringLitExpr = S.EStringLit <$> stringLiteral

memberAccess :: Parser S.MAccess
memberAccess = try dotMember <|> bracketMember

dotMember :: Parser S.MAccess
dotMember = do
  _ <- symbol "."
  S.MDotAccess <$> identifier

bracketMember :: Parser S.MAccess
bracketMember = S.MBracketAccess <$> between (symbol "[") (symbol "]") expr

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

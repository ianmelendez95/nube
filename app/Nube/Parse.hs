module Nube.Parse
  ( Parser,
    stringLitExpr,
    parseJsFile,
    parseJsContent,
    identifier,
    dotMember,
    bracketMember,
    expr,
    statement,
    switchStmt,
    function,
    runParser,
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlpha, isAlphaNum)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Nube.Context (NContext (..), ctxAddFnM, ctxGetIsFnM)
import Nube.Parser (Parser, runParser)
import Nube.Syntax qualified as S
import System.FilePath (takeBaseName)
import Text.Megaparsec
  ( MonadParsec (takeWhileP, try),
    between,
    choice,
    errorBundlePretty,
    many,
    manyTill,
    manyTill_,
    optional,
    satisfy,
    sepBy,
    (<|>),
    lookAhead,
  )
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L
  ( charLiteral,
    decimal,
    lexeme,
    skipBlockComment,
    skipLineComment,
    space,
    symbol,
  )

-- import Text.Megaparsec.Debug (dbg)

parseJsFile :: FilePath -> IO (S.Script, NContext)
parseJsFile path = parseJsContent path =<< TIO.readFile path

parseJsContent :: (MonadFail m) => FilePath -> T.Text -> m (S.Script, NContext)
parseJsContent path content = do
  let run_res = runParser (NContext []) jsFunctions path content
  (result, ctx) <- either (fail . errorBundlePretty) pure run_res
  pure (S.Script (T.pack $ takeBaseName path) result, ctx)

jsFunctions :: Parser [S.Fn]
jsFunctions = many function

function :: Parser S.Fn
function = do
  _ <- symbol "function"
  S.Fn <$> parseFnName <*> fn_parameters <*> fn_body
  where
    fn_parameters :: Parser [T.Text]
    fn_parameters = between (symbol "(") (symbol ")") $ sepBy identifier (symbol ",")

    fn_body :: Parser [S.Stmt]
    fn_body = between (symbol "{") (symbol "}") $ many statement

    parseFnName :: Parser T.Text
    parseFnName = do
      fname <- identifier
      is_existing <- ctxGetIsFnM fname
      if is_existing
        then fail $ "Duplicate function names: " ++ show fname
        else do
          ctxAddFnM fname
          return fname

statement :: Parser S.Stmt
statement = (switchStmt <|> try const_assign <|> return_stmt <|> try assign_or_expr) <* symbol ";"
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

    assign_or_expr :: Parser S.Stmt
    assign_or_expr = do
      lhs <- expr
      eq_sign <- optional $ symbol "="
      case eq_sign of
        Nothing -> pure $ S.SExpr lhs
        Just _ -> S.SAssign lhs <$> expr

switchStmt :: Parser S.Stmt
switchStmt = do
  _ <- symbol "switch"
  S.SSwitch <$> parens expr <*> braces (many caseStmt)

caseStmt :: Parser S.SCase
caseStmt = do 
  _ <- symbol "case"
  state_num <- lexeme L.decimal
  _ <- symbol ":"
  S.SCase state_num <$> many statement

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
          S.ENumberLit <$> L.decimal,
          listLitExpr
        ]

exprTable :: [[Operator Parser S.Expr]]
exprTable =
  [ [binary "+" (S.EInfix S.IPlus)]
  ]
  where
    binary :: T.Text -> (S.Expr -> S.Expr -> S.Expr) -> Operator Parser S.Expr
    binary name f = InfixL (f <$ symbol name)

callParens :: Parser [S.Expr]
callParens = parens (sepBy expr (symbol ","))

parens :: Parser a -> Parser a 
parens = between (symbol "(") (symbol ")") 

braces :: Parser a -> Parser a 
braces = between (symbol "{") (symbol "}") 

varExpr :: Parser S.Expr
varExpr = do
  id_txt <- identifier 
  if id_txt `elem` keywords
    then fail $ "Keyword is reserved: " ++ show id_txt
    else pure $ S.EVar id_txt

listLitExpr :: Parser S.Expr
listLitExpr = S.EListLit <$> between (symbol "[") (symbol "]") (sepBy expr (symbol ","))

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
identifier =
  do
    T.cons
    <$> satisfy (\c -> c == '_' || isAlpha c)
    <*> takeWhileP (Just "identifier char") (\c -> c == '_' || isAlphaNum c)

keywords :: [T.Text]
keywords = ["case"]

stringLiteral :: Parser T.Text
stringLiteral = stringLiteral' (char '\'') <|> stringLiteral' (char '"')

stringLiteral' :: Parser a -> Parser T.Text
stringLiteral' quotes = T.pack <$> (quotes >> manyTill L.charLiteral quotes)

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

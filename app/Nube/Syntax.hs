module Nube.Syntax
  ( Script (..),
    Fn (..),
    Stmt (..),
    SCase (..),
    Expr (..),
    MAccess (..),
    mapScriptStmtsM,
    mapScriptFnsM,
    scriptText,
    fnText,
    scriptFns',
    fnStmts',
    IOp (..),
    dotMemberExpr,
    dotMembers,
    mkBracketMember,
  )
where

import Control.Lens (Traversal', traversal)
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Symbols.Ascii

data Script = Script
  { scriptName :: T.Text, -- file basename
    scriptFns :: [Fn]
  }

data Fn = Fn
  { fnName :: T.Text,
    fnParams :: [T.Text],
    fnStmts :: [Stmt]
  }
  deriving (Eq)

data Stmt
  = SConst T.Text Expr
  | SAssign Expr Expr
  | SReturn Expr
  | SExpr Expr
  | SSwitch Expr [SCase]
  | SBreak
  deriving (Eq)

data SCase = SCase Int [Stmt] deriving (Eq)

data Expr
  = EVar T.Text
  | EListLit [Expr]
  | EStringLit T.Text
  | ENumberLit Int
  | ECall Expr [Expr]
  | EMember Expr MAccess
  | EInfix IOp Expr Expr
  deriving (Eq)

data IOp = IPlus
  deriving (Show, Eq)

data MAccess
  = MDotAccess T.Text
  | MBracketAccess Expr
  deriving (Show, Eq)

instance Show Script where
  show = show . pretty

instance Show Fn where
  show = show . pretty

instance Show Stmt where
  show = show . pretty

instance Show Expr where
  show = show . pretty

instance Show SCase where
  show = show . pretty

instance Pretty Script where
  pretty = pretty . scriptText

instance Pretty Fn where
  pretty (Fn name params body) =
    vsep [nest 2 (vsep [fn_sig, pretty_body]), rbrace]
    where
      fn_sig :: Doc a
      fn_sig = "function" <+> pretty name <> params_doc <+> lbrace

      params_doc :: Doc a
      params_doc = parens $ prettyCSV params

      pretty_body :: Doc a
      pretty_body = vsep (map pretty body)

instance Pretty Stmt where
  pretty (SConst var rhs) = "const" <+> pretty var <+> equals <+> pretty rhs <> semi
  pretty (SReturn rhs) = "return" <+> pretty rhs <> semi
  pretty (SExpr e) = pretty e <> semi
  pretty (SAssign lhs rhs) = pretty lhs <+> equals <+> pretty rhs <> semi
  pretty (SSwitch match cases) =
    nest 2 (vsep $ "switch" <+> parens (pretty match) <+> lbrace : map pretty cases) <> line <> rbrace
  pretty SBreak = "break" <> semi

instance Pretty SCase where
  pretty (SCase state stmts) =
    nest 2 (vsep $ "case" <+> pretty state <> colon : map pretty stmts)

instance Pretty Expr where
  pretty (EVar v) = pretty v
  pretty (EStringLit s) = squotes (pretty s)
  pretty (ENumberLit n) = pretty $ T.show n
  pretty (ECall lhs args) = pretty lhs <> parens (prettyCSV args)
  pretty (EMember lhs rhs) = pretty lhs <> pretty rhs
  pretty (EInfix op lhs rhs) = pretty lhs <+> pretty op <+> pretty rhs
  pretty (EListLit xs) = prettyList xs

instance Pretty MAccess where
  pretty (MDotAccess v) = dot <> pretty v
  pretty (MBracketAccess rhs) = brackets $ pretty rhs

instance Pretty IOp where
  pretty IPlus = "+"

prettyCSV :: (Pretty a) => [a] -> Doc b
prettyCSV xs = concatWith (\l r -> l <> comma <+> r) (map pretty xs)

scriptFns' :: Traversal' Script [Fn]
scriptFns' = traversal (\fnsFnM (Script s_name s_fns) -> Script s_name <$> fnsFnM s_fns)

fnStmts' :: Traversal' Fn [Stmt]
fnStmts' = traversal (\stmtsFnM (Fn f_name f_args f_stmts) -> Fn f_name f_args <$> stmtsFnM f_stmts)

mapScriptStmtsM :: (Monad m) => (Stmt -> m Stmt) -> Script -> m Script
mapScriptStmtsM = mapScriptFnsM . mapFnStmtsM

mapScriptFnsM :: (Monad m) => (Fn -> m Fn) -> Script -> m Script
mapScriptFnsM fn_m (Script s_name s_fns) =
  Script s_name <$> mapM fn_m s_fns

mapFnStmtsM :: (Monad m) => (Stmt -> m Stmt) -> Fn -> m Fn
mapFnStmtsM stmt_fn_m (Fn f_name f_params f_stmts) =
  Fn f_name f_params <$> mapM stmt_fn_m f_stmts

scriptText :: Script -> T.Text
scriptText (Script name funcs) = T.unlines $ name : map fnText funcs

fnText :: Fn -> T.Text
fnText (Fn name params body) = "function " <> name <> params_text <> " {\n  " <> body_text <> ";\n}"
  where
    params_text = "(" <> T.intercalate ", " params <> ")"
    body_text = T.intercalate ";\n  " (map stmtText body)

stmtText :: Stmt -> T.Text
stmtText (SConst var rhs) = "const " <> var <> " = " <> exprText rhs
stmtText (SReturn rhs) = "return " <> exprText rhs
stmtText (SExpr e) = exprText e
stmtText (SAssign lhs rhs) = exprText lhs <> " = " <> exprText rhs
stmtText (SSwitch match cases) = undefined

exprText :: Expr -> T.Text
exprText (EVar v) = v
exprText (EStringLit s) = "'" <> s <> "'"
exprText (ENumberLit n) = T.show n
exprText (ECall lhs args) = exprText lhs <> csExprs "(" ")" args
exprText (EMember lhs rhs) = exprText lhs <> accessText rhs
exprText (EInfix op lhs rhs) = exprText lhs <> " " <> opText op <> " " <> exprText rhs
exprText (EListLit xs) = csExprs "[" "]" xs

csExprs :: T.Text -> T.Text -> [Expr] -> T.Text
csExprs bra cket xs = bra <> T.intercalate ", " (map exprText xs) <> cket

accessText :: MAccess -> T.Text
accessText (MDotAccess v) = "." <> v
accessText (MBracketAccess rhs) = "[" <> exprText rhs <> "]"

dotMembers :: Expr -> [T.Text] -> Expr
dotMembers = foldl dotMemberExpr

dotMemberExpr :: Expr -> T.Text -> Expr
dotMemberExpr lhs var = EMember lhs (MDotAccess var)

mkBracketMember :: Expr -> Expr -> Expr
mkBracketMember lhs member_epxr = EMember lhs (MBracketAccess member_epxr)

opText :: IOp -> T.Text
opText IPlus = "+"

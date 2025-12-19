module Nube.Syntax
  ( Script (..),
    Fn (..),
    Stmt (..),
    SCase (..),
    Expr (..),
    MAccess (..),
    mapScriptStmtsM,
    mapScriptFnsM,
    scriptFns',
    fnStmts',
    IOp (..),
    dotMemberExpr,
    dotMembers,
    mkBracketMember,
    caseStmts'
  )
where

import Control.Lens (Traversal', traversal)
import Data.Text qualified as T
import Prettyprinter

data Script = Script
  { scriptName :: T.Text, -- file basename
    scriptFns :: [Fn]
  } deriving Eq

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
  | EArrLit [Expr]
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
  pretty (Script _ funcs) = vsep $ map pretty funcs

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
  pretty (EArrLit xs) = prettyList xs

instance Pretty MAccess where
  pretty (MDotAccess v) = dot <> pretty v
  pretty (MBracketAccess rhs) = brackets $ pretty rhs

instance Pretty IOp where
  pretty IPlus = "+"

prettyCSV :: (Pretty a) => [a] -> Doc b
prettyCSV xs = concatWith (\l r -> l <> comma <+> r) (map pretty xs)

scriptFns' :: Traversal' Script [Fn]
scriptFns' = traversal $ \fnsFnM (Script s_name s_fns) -> Script s_name <$> fnsFnM s_fns

fnStmts' :: Traversal' Fn [Stmt]
fnStmts' = traversal $ \stmtsFnM (Fn f_name f_args f_stmts) -> Fn f_name f_args <$> stmtsFnM f_stmts

caseStmts' :: Traversal' SCase [Stmt]
caseStmts' = traversal $ \stmtsFnM (SCase case_i case_stmts) -> SCase case_i <$> stmtsFnM case_stmts

mapScriptStmtsM :: (Monad m) => (Stmt -> m Stmt) -> Script -> m Script
mapScriptStmtsM = mapScriptFnsM . mapFnStmtsM

mapScriptFnsM :: (Monad m) => (Fn -> m Fn) -> Script -> m Script
mapScriptFnsM fn_m (Script s_name s_fns) =
  Script s_name <$> mapM fn_m s_fns

mapFnStmtsM :: (Monad m) => (Stmt -> m Stmt) -> Fn -> m Fn
mapFnStmtsM stmt_fn_m (Fn f_name f_params f_stmts) =
  Fn f_name f_params <$> mapM stmt_fn_m f_stmts

dotMembers :: Expr -> [T.Text] -> Expr
dotMembers = foldl dotMemberExpr

dotMemberExpr :: Expr -> T.Text -> Expr
dotMemberExpr lhs var = EMember lhs (MDotAccess var)

mkBracketMember :: Expr -> Expr -> Expr
mkBracketMember lhs member_epxr = EMember lhs (MBracketAccess member_epxr)


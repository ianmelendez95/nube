{-# LANGUAGE OverloadedStrings #-}

module JS.Transpile where

import Data.Text qualified as T
import JS.Syntax qualified as S

transpileStatement :: S.Stmt -> S.Stmt
transpileStatement (S.SReturn e) = S.SExpr (S.ECall (S.EMember (S.EVar "ctx") (S.EDotAccess "return")) [e])
transpileStatement _ = undefined

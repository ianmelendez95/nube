module JS.TestTranspile (jsTranspileSpec) where

import JS.Syntax qualified as S
import JS.Transpile
  ( transpileStatement,
  )
import Test.Hspec
  ( SpecWith (..),
    describe,
    it,
    shouldBe,
    xdescribe,
  )

jsTranspileSpec =
  describe "transpileStmt" $ do
    it "transpiles return" $ do
      let res = transpileStatement (S.SReturn (S.EVar "x"))
      res `shouldBe` S.SExpr (S.ECall (S.EMember (S.EVar "ctx") (S.EDotAccess "return")) [S.EVar "x"])

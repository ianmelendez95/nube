module JS.TestTranspile (jsTranspileSpec) where

import Data.Either (either)
import JS.Syntax qualified as S
import JS.Transpile
  ( ctx_var,
    transpileStatement,
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
      res `shouldBeRight` S.SExpr (S.ECall (S.EMember ctx_var (S.MDotAccess "return")) [S.EVar "x"])

shouldBeRight lhs = (lhs `shouldBe`) . Right

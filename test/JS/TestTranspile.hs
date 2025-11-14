module JS.TestTranspile (jsTranspileSpec) where

import Data.Either (either)
import JS.Syntax qualified as S
import JS.Transpile
  ( ctx_var_name,
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
    it "transpiles simple return var" $ do
      let res = transpileStatement (S.SReturn (S.EVar "x"))
      res
        `shouldBeRight` S.SExpr
          ( S.ECall
              ( S.EMember
                  ctx_var_name
                  (S.MDotAccess "return")
              )
              [S.dotMembers (S.EVar "_ctx") ["frame", "x"]]
          )

shouldBeRight lhs = (lhs `shouldBe`) . Right

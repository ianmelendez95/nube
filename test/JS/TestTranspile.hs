module JS.TestTranspile (jsTranspileSpec) where

import Data.Either (either)
import JS.Syntax qualified as S
import JS.Transpile
  ( TContext (..),
    ctx_var_name,
    runTranspiler,
    transpileSem,
    transpileStatement,
  )
import Test.Hspec
  ( SpecWith (..),
    describe,
    it,
    shouldBe,
    xdescribe,
  )

jsTranspileSpec = do
  -- describe "transpileSem" $ do
  --   it "transpiles valid var" $ do
  --     let res = transpileStatement test_context (S.EVar "x")
  --     res `shouldBeRight` S.EVar "x"

  --   it "transpiles with 'fn exists' error" $ do
  --     let res = transpileStatement test_context (S.EVar "capitalizeWord")
  --     res `shouldBe` Left "fn exists"

  describe "transpileStmt" $ do
    it "transpiles simple return var" $ do
      let res = transpileStatement test_context (S.SReturn (S.EVar "x"))
      res
        `shouldBeRight` S.SExpr
          ( S.ECall
              ( S.EMember
                  ctx_var_name
                  (S.MDotAccess "return")
              )
              [S.dotMembers (S.EVar "_ctx") ["frame", "x"]]
          )

    it "transpiles var dot member" $ do
      let res = transpileStatement test_context (S.SReturn (S.dotMemberExpr (S.EVar "foo") "bar"))
      res
        `shouldBeRight` S.SExpr
          ( S.ECall
              ( S.EMember
                  ctx_var_name
                  (S.MDotAccess "return")
              )
              [S.dotMembers (S.EVar "_ctx") ["frame", "foo", "bar"]]
          )

test_context :: TContext
test_context = TContext ["capitalizeWords", "capitalizeWord"]

shouldBeRight lhs = (lhs `shouldBe`) . Right

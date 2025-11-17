module Test.Nube.Transpile (jsTranspileSpec) where

import Data.Either (either)
import Data.Text qualified as T
import Nube.Compiler (CContext (CContext))
import Nube.FrameRefs (tStatement)
import Nube.JSCtx
  ( ctx_var_name,
  )
import Nube.Parse qualified as P
import Nube.Syntax qualified as S
import Test.Example.CapitalizeTwoWords (capitalizeTwoWords_fn_ast)
import Test.Hspec
  ( Expectation,
    SpecWith (..),
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldSatisfy,
    xdescribe,
  )
import Test.Nube.Parse (testParser)
import Test.Util.Nube (testCompiler)

jsTranspileSpec = do
  describe "transpileStatement" $ do
    it "transpiles simple return var" $ do
      let res = testCompiler test_context (tStatement (S.SReturn (S.EVar "x")))
      res
        `shouldBe` S.SExpr
          ( S.ECall
              ( S.EMember
                  ctx_var_name
                  (S.MDotAccess "return")
              )
              [S.dotMembers (S.EVar "_ctx") ["frame", "x"]]
          )

    it "transpiles var dot member" $ do
      let res = testCompiler test_context (tStatement (S.SReturn (S.dotMemberExpr (S.EVar "foo") "bar")))
      res
        `shouldBe` S.SExpr
          ( S.ECall
              ( S.EMember
                  ctx_var_name
                  (S.MDotAccess "return")
              )
              [S.dotMembers (S.EVar "_ctx") ["frame", "foo", "bar"]]
          )

test_context :: CContext
test_context = CContext ["capitalizeWords", "capitalizeWord"]

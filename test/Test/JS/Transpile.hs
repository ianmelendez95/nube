module Test.JS.Transpile (jsTranspileSpec) where

import Compile.JSCtx
  ( ctx_var_name,
  )
import Data.Either (either)
import Data.Text qualified as T
import JS.Parse qualified as P
import JS.Syntax qualified as S
import JS.Transpile
  ( Compiler,
    ContSplit (..),
    TContext (..),
    runCompiler,
    transpileStatement,
  )
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
import Test.JS.Parse (testParser)
import Test.Util.Compile

jsTranspileSpec = do
  describe "transpileStatement" $ do
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

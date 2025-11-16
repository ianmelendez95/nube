module Test.JS.Transpile (jsTranspileSpec) where

import Data.Either (either)
import Data.Text qualified as T
import JS.Parse qualified as P
import JS.Syntax qualified as S
import JS.Transpile
  ( ContSplit (..),
    TContext (..),
    Transpiler,
    ctx_var_name,
    runTranspiler,
    splitStmtContinuations,
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

jsTranspileSpec = do
  describe "splitStmtContinuations" $ do
    it "splits capitalizeTwoWords stmts" $ do
      let ctx = TContext ["capitalizeTwoWords", "capitalizeWord"]
          res = testTranspiler ctx (splitStmtContinuations (S.fnStmts capitalizeTwoWords_fn_ast))
      mapM_ (printContSplit "  ") res
      -- res `shouldSatisfy` [3, 1, 1, 1]
      length res `shouldBe` 3

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

printContSplit :: String -> ContSplit -> IO ()
printContSplit prefix (ContBlock stmts) = do
  putStrLn "--- BLOCK ---"
  mapM_ (\stmt -> putStr prefix >> print stmt) stmts
  putStrLn "--- END BLOCK ---"
printContSplit prefix cont_call = putStrLn "--- CONT ---" >> putStr prefix >> print cont_call

testTranspiler :: TContext -> Transpiler a -> a
testTranspiler ctx = either error id . runTranspiler ctx

test_context :: TContext
test_context = TContext ["capitalizeWords", "capitalizeWord"]

shouldBeRight :: (Show a1, Show a2, Eq a1, Eq a2) => Either a1 a2 -> a2 -> Expectation
shouldBeRight lhs = (lhs `shouldBe`) . Right

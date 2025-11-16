module Test.Compile.Cont (jsCompileContSpec) where

import Compile.JSCtx
import Data.Either (either)
import Data.Text qualified as T
import JS.Parse qualified as P
import JS.Syntax qualified as S
import JS.Transpile
  ( ContSplit (..),
    TContext (..),
    Transpiler,
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
import Test.Util.Compile

jsCompileContSpec = do
  describe "splitStmtContinuations" $ do
    it "splits capitalizeTwoWords stmts" $ do
      let ctx = TContext ["capitalizeTwoWords", "capitalizeWord"]
          res = testTranspiler ctx (splitStmtContinuations (S.fnStmts capitalizeTwoWords_fn_ast))
      -- print capitalizeTwoWords_fn_ast
      -- mapM_ (printContSplit "  ") res
      -- res `shouldSatisfy` [3, 1, 1, 1]
      length res `shouldBe` 3
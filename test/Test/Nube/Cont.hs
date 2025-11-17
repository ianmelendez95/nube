module Test.Nube.Cont (jsCompileContSpec) where

import Data.Either (either)
import Data.Text qualified as T
import Nube.Compiler
  ( CContext (..),
  )
import Nube.Cont
import Nube.Cont
  ( splitStmtContinuations,
  )
import Nube.JSCtx
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
import Test.JS.Parse (testParser)
import Test.Util.Nube (testCompiler)

jsCompileContSpec = do
  describe "splitStmtContinuations" $ do
    it "splits capitalizeTwoWords stmts" $ do
      let ctx = CContext ["capitalizeTwoWords", "capitalizeWord"]
          res = testCompiler ctx (splitStmtContinuations (S.fnStmts capitalizeTwoWords_fn_ast))
      -- print capitalizeTwoWords_fn_ast
      -- mapM_ (printContSplit "  ") res
      -- res `shouldSatisfy` [3, 1, 1, 1]
      length res `shouldBe` 3

printContSplit :: String -> ContSplit -> IO ()
printContSplit prefix (ContBlock stmts) = do
  putStrLn "--- BLOCK ---"
  mapM_ (\stmt -> putStr prefix >> print stmt) stmts
  putStrLn "--- END BLOCK ---"
printContSplit prefix cont_call = putStrLn "--- CONT ---" >> putStr prefix >> print cont_call

module Test.Nube.Cont (jsCompileContSpec) where

import Data.Either (either)
import Data.Text qualified as T
import Nube.Cont
import Nube.Cont
  ( splitFnContinuations,
    splitStmtContinuations,
  )
import Nube.Context
  ( NContext (..),
  )
import Nube.JSCtx
import Nube.Parse qualified as P
import Nube.Syntax qualified as S
import Test.Example.CapitalizeTwoWords (capitalizeTwoWords_fn_ast, capitalizeTwoWords_fn_text)
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

jsCompileContSpec = do
  describe "splitFnContinuations" $ do
    it "splits ctw fns" $ do
      let ctx = NContext ["capitalizeTwoWords", "capitalizeWord"]
          res = testCompiler ctx (splitFnContinuations capitalizeTwoWords_fn_ast)
       in do
            putStrLn "--- BEFORE ---" >> mapM_ print res
            print capitalizeTwoWords_fn_ast
            putStrLn "--- AFTER  ---" >> mapM_ print res
            length res `shouldBe` 3

  describe "splitStmtContinuations" $ do
    it "splits capitalizeTwoWords stmts" $ do
      let ctx = NContext ["capitalizeTwoWords", "capitalizeWord"]
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

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
import Test.Util.Nube (testCompiler)
import Test.Util.Parse (runParser, testParser)

jsCompileContSpec = do
  describe "splitFnContinuations" $ do
    it "splits ctw fns" $ do
      let ctx = NContext ["capitalizeTwoWords", "capitalizeWord"]
          res = testCompiler ctx (splitFnContinuations capitalizeTwoWords_fn_ast)
       in do
            length res `shouldBe` 3
            let [prim_fn, cont_fn1, cont_fn2] = res
            prim_fn `shouldBe` capitalizeTwoWords_fn_prim_ast

  describe "splitStmtContinuations" $ do
    it "splits capitalizeTwoWords stmts" $ do
      let ctx = NContext ["capitalizeTwoWords", "capitalizeWord"]
          res = testCompiler ctx (splitStmtContinuations "capitalizeTwoWords" (S.fnStmts capitalizeTwoWords_fn_ast))
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

capitalizeTwoWords_fn_prim_ast :: S.Fn
capitalizeTwoWords_fn_prim_ast = runParser P.function capitalizeTwoWords_fn_prim_text

capitalizeTwoWords_fn_prim_text :: T.Text
capitalizeTwoWords_fn_prim_text =
  T.pack
    "function capitalizeTwoWords(_ctx) {\n\
    \  _ctx.frame.string = _ctx.args[0];\n\
    \  const words = string.split(' ');\n\
    \  const word1 = words[0];\n\
    \  const word2 = words[1];\n\
    \  _ctx.call('capitalizeWord', [word1], 'capitalizeTwoWordsC1');\n\
    \}"
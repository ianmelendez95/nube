module Test.Nube.St (testSt) where

import Data.Either (either)
import Data.Text qualified as T
import Nube.St (compileScriptSt)
import Nube.Context
  ( NContext (..),
  )
import Nube.JSCtx
import Nube.Parse qualified as P
import Nube.St
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
import Test.Files (parseTestFile, readTestFile)

testSt = do
  describe "Nube.St" $ do 
    describe "compileScriptSt" $ do
      it "compiles capitalizeTwoWords fn state" $ do
        before_script <- parseTestFile "capitalizeTwoWords/capitalizeTwoWords_fn.js" (P.script "capitalizeTwoWords_fn.js")
        after_script_js <- T.strip <$> readTestFile "capitalizeTwoWords/capitalizeTwoWords_state_fn.js"

        let ctx = NContext ["capitalizeTwoWords", "capitalizeWord"]

            res :: S.Script
            res = testCompiler ctx (compileScriptSt before_script)

            res_js = show res

        res_js `shouldBe` T.unpack after_script_js

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
    \  _ctx.callCC('capitalizeWord', [word1], 'capitalizeTwoWordsC1');\n\
    \}"

capitalizeTwoWords_fn_state_text :: T.Text
capitalizeTwoWords_fn_state_text = 
  T.pack 
    "function capitalizeTwoWords(_ctx) {\n\
    \  switch (_ctx.state) {\n\
    \    const words = string.split(' ');\n\
    \    const word1 = words[0];\n\
    \    const word2 = words[1];\n\
    \    const capitalizedWord1 = capitalizeWord(word1);\n\
    \    const capitalizedWord2 = capitalizeWord(word2);\n\
    \    return capitalizedWord1 + ' ' + capitalizedWord2;\n\
    \  }\n\
    \}"

module Test.Nube.Syntax (testSyntax) where 

import Data.Either (either)
import Data.Text qualified as T
import Nube.Context
  ( NContext (..),
  )
import Nube.JSCtx
import Nube.Parse qualified as P
import Nube.St
  (
  )
import Nube.Syntax
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
import Test.Files (readTestFile)
import Prettyprinter

testSyntax = do
  describe "Nube.Syntax" $ do
    describe "Pretty" $ do 
      it "indents simple function" $ do 
        T.show (pretty _test_return_fn) `shouldBe` _test_return_fn_txt

      it "indents simple switch" $ do 
        T.show (pretty _test_switch_fn) `shouldBe` _test_switch_fn_txt

      it "reprints capitalizeTwoWords_state" $ do 
        fn_txt <- T.strip <$> _capitalizeTwoWords_state_fn_text
        parsed <- testParser P.function fn_txt
        (T.strip . T.show . pretty $ parsed) `shouldBe` fn_txt 

      it "array lit no space" $ do 
        -- return _ctx.callCC('capitalizeWord', [ _ctx.frame.word2 ], 'capitalizeTwoWords', 2);
        let arr_mem :: Expr
            arr_mem = EArrLit [EMember (EVar "_ctx") (MDotAccess "frame")]
        show arr_mem `shouldBe` "[_ctx.frame]"

_capitalizeTwoWords_state_fn_text :: IO T.Text 
_capitalizeTwoWords_state_fn_text = readTestFile "capitalizeTwoWords/capitalizeTwoWords_state_fn.js"

_test_return_fn :: Fn
_test_return_fn = Fn "testFn" ["x", "y"] [SReturn (EInfix IPlus (EVar "x") (EVar "y"))]

_test_return_fn_txt :: T.Text
_test_return_fn_txt = 
  "function testFn(x, y) {\n\
  \  return x + y;\n\
  \}"

_test_switch_fn_txt :: T.Text
_test_switch_fn_txt = 
  "function testFn(state, x, y) {\n\
  \  switch (state) {\n\
  \    case 0:\n\
  \      return x;\n\
  \      break;\n\
  \    case 1:\n\
  \      return y;\n\
  \      break;\n\
  \  }\n\
  \}"

_test_switch_fn :: Fn
_test_switch_fn = Fn "testFn" ["state", "x", "y"] [_switch]
  where 
    _switch = SSwitch (EVar "state") [_case_0, _case_1]
    _case_0 = SCase 0 [SReturn (EVar "x"), SBreak]
    _case_1 = SCase 1 [SReturn (EVar "y"), SBreak]




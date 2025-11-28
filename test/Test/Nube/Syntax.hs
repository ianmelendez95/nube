module Test.Nube.Syntax (testSyntax) where 

import Data.Either (either)
import Data.Text qualified as T
import Nube.Cont
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
import Prettyprinter

testSyntax = do
  describe "Nube.Syntax" $ do
    describe "stmtTextI" $ do 
      it "indents simple switch" $ do 
        let test_fn = Fn "testFn" ["x", "y"] [SReturn (EInfix IPlus (EVar "x") (EVar "y"))]
        T.show (pretty _test_return_fn) `shouldBe` _test_return_fn_txt

_test_return_fn :: Fn
_test_return_fn = Fn "testFn" ["x", "y"] [SReturn (EInfix IPlus (EVar "x") (EVar "y"))]

_test_return_fn_txt :: T.Text
_test_return_fn_txt = 
  "function testFn(x, y) {\n\
  \  return x + y;\n\
  \}"


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

testSyntax = do
  describe "stmtTextI" $ do 
    it "indents simple switch" $ do 
      undefined


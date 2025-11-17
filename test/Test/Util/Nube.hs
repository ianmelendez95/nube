module Test.Util.Nube
  ( testCompiler,
    shouldBeRight,
  )
where

import Nube
  ( runCompiler,
  )
import Nube.Compiler (Compiler)
import Nube.Context (NContext (..))
import Test.Hspec
  ( Expectation,
    shouldBe,
  )

testCompiler :: NContext -> Compiler a -> a
testCompiler ctx = either error id . runCompiler ctx

shouldBeRight :: (Show a1, Show a2, Eq a1, Eq a2) => Either a1 a2 -> a2 -> Expectation
shouldBeRight lhs = (lhs `shouldBe`) . Right
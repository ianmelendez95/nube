module Test.Util.Compile
  ( testCompiler,
    shouldBeRight,
  )
where

import Compile
  ( runCompiler,
  )
import Nube.Compiler (CContext (..), Compiler)
import Test.Hspec
  ( Expectation,
    shouldBe,
  )

testCompiler :: CContext -> Compiler a -> a
testCompiler ctx = either error id . runCompiler ctx

shouldBeRight :: (Show a1, Show a2, Eq a1, Eq a2) => Either a1 a2 -> a2 -> Expectation
shouldBeRight lhs = (lhs `shouldBe`) . Right
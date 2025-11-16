module Test.Util.Compile
  ( testCompiler,
    shouldBeRight,
  )
where

import JS.Transpile
  ( Compiler,
    TContext (..),
    runCompiler,
    transpileStatement,
  )
import Test.Hspec
  ( Expectation,
    shouldBe,
  )

testCompiler :: TContext -> Compiler a -> a
testCompiler ctx = either error id . runCompiler ctx

shouldBeRight :: (Show a1, Show a2, Eq a1, Eq a2) => Either a1 a2 -> a2 -> Expectation
shouldBeRight lhs = (lhs `shouldBe`) . Right
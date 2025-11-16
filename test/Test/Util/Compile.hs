module Test.Util.Compile
  ( testTranspiler,
    shouldBeRight,
  )
where

import JS.Transpile
  ( TContext (..),
    Transpiler,
    runTranspiler,
    splitStmtContinuations,
    transpileStatement,
  )
import Test.Hspec
  ( Expectation,
    shouldBe,
  )

testTranspiler :: TContext -> Transpiler a -> a
testTranspiler ctx = either error id . runTranspiler ctx

shouldBeRight :: (Show a1, Show a2, Eq a1, Eq a2) => Either a1 a2 -> a2 -> Expectation
shouldBeRight lhs = (lhs `shouldBe`) . Right
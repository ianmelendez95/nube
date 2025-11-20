module Test.Nube.Rename (jsTranspileSpec) where

import Data.Either (either)
import Data.Text qualified as T
import Nube.Context (NContext (NContext))
import Nube.JSCtx
  ( ctx_var_name,
  )
import Nube.Parse qualified as P
import Nube.Rename (rExpr, renameInStatement)
import Nube.Syntax qualified as S
import Test.Example.CapitalizeTwoWords (capitalizeTwoWords_fn_ast)
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
import Test.Util.Parse (testParser)

jsTranspileSpec = do
  describe "renameInStatement" $ do
    it "renames simple return var" $ do
      let res = testCompiler test_context (renameInStatement (S.SReturn (S.EVar "x")))
      res
        `shouldBe` S.SExpr
          ( S.ECall
              ( S.EMember
                  ctx_var_name
                  (S.MDotAccess "return")
              )
              [S.dotMembers (S.EVar "_ctx") ["frame", "x"]]
          )

    it "renames var dot member" $ do
      let res = testCompiler test_context (renameInStatement (S.SReturn (S.dotMemberExpr (S.EVar "foo") "bar")))
      res
        `shouldBe` S.SExpr
          ( S.ECall
              ( S.EMember
                  ctx_var_name
                  (S.MDotAccess "return")
              )
              [S.dotMembers (S.EVar "_ctx") ["frame", "foo", "bar"]]
          )

  describe "rExpr" $ do
    it "renames in call args" $ do
      let test_expr :: S.Expr
          test_expr =
            S.ECall
              (S.EMember (S.EVar "_ctx") (S.MDotAccess "call"))
              [ S.EStringLit "capitalizeWord",
                S.EListLit [S.EVar "word1"],
                S.EStringLit "capitalizeWordsC2"
              ]
          res = testCompiler test_context (rExpr test_expr)
      print test_expr
      res
        `shouldBe` S.ECall
          ( S.EMember
              (S.EVar "_ctx")
              (S.MDotAccess "call")
          )
          [ S.EStringLit "capitalizeWord",
            S.EListLit [S.EMember (S.EMember (S.EVar "_ctx") (S.MDotAccess "frame")) (S.MDotAccess "word1")],
            S.EStringLit "capitalizeWordsC2"
          ]

test_context :: NContext
test_context = NContext ["capitalizeWords", "capitalizeWord"]

module Test.JS.Parse
  ( jsParseSpec,
    testParser,
  )
where

import Data.Text
  ( Text,
    pack,
  )
import JS.Parse
  ( Parser (..),
    bracketMember,
    dotMember,
    expr,
    function,
    identifier,
    statement,
    stringLitExpr,
  )
import JS.Syntax
  ( Expr (..),
    Fn (..),
    IOp (..),
    MAccess (..),
    Stmt (..),
  )
import Test.Example.CapitalizeTwoWords
  ( capitalizeTwoWords_ast,
    capitalizeTwoWords_text,
  )
import Test.Hspec
  ( SpecWith (..),
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldSatisfy,
    xdescribe,
  )
import Test.Util.Parse
  ( runParser,
    testParser,
  )

jsParseSpec = do
  describe "function" $ do
    it "parses simple function" $ do
      res <- testParser function "function foo(x) { return x; }"
      res `shouldBe` Fn "foo" ["x"] [SReturn (EVar "x")]

    it "parses capitalizeTwoWords" $ do
      (Fn name params stmts) <- testParser function capitalizeTwoWords_text
      name `shouldBe` "capitalizeTwoWords"
      params `shouldBe` ["string"]
      length stmts `shouldBe` 6

  describe "statement" $ do
    it "parses assign statement" $ do
      res <- testParser statement "const word = 'hello';"
      res `shouldBe` SConst "word" (EStringLit "hello")

    it "parses return statement" $ do
      res <- testParser statement "return x;"
      res `shouldBe` SReturn (EVar "x")

  describe "expr" $ do
    it "parses string literal" $ do
      res <- testParser expr "\"hello world!\""
      res `shouldBe` EStringLit "hello world!"

    let string_split = EMember (EVar "string") (MDotAccess "split")

    it "parses dot member access" $ do
      res <- testParser expr "string.split"
      res `shouldBe` string_split

    it "parses bracket member access" $ do
      res <- testParser expr "string['split']"
      res `shouldBe` EMember (EVar "string") (MBracketAccess (EStringLit "split"))

    it "parses dot member call" $ do
      res <- testParser expr "string.split()"
      res `shouldBe` ECall string_split []

    it "parses three level dot member call" $ do
      res <- testParser expr "string.split().length"
      res `shouldBe` EMember (ECall string_split []) (MDotAccess "length")

    it "parses simple var arg call" $ do
      res <- testParser expr "hello(myVar)"
      res `shouldBe` ECall (EVar "hello") [EVar "myVar"]

    it "parses simple add" $ do
      res <- testParser expr "4 + 2"
      res `shouldBe` EInfix IPlus (ENumberLit 4) (ENumberLit 2)

    it "parses bracket member, dot member, then call with number" $ do
      res <- testParser expr "words[0].slice(1)"
      res
        `shouldBe` ECall
          ( EMember
              (EMember (EVar "words") (MBracketAccess (ENumberLit 0)))
              (MDotAccess "slice")
          )
          [ENumberLit 1]

  describe "identifier" $ do
    it "returns the identifier" $ do
      id <- testParser identifier "hello"
      id `shouldBe` "hello"

  describe "dotMember" $ do
    it "returns the property name" $ do
      prop <- testParser dotMember ".someProp"
      prop `shouldBe` MDotAccess "someProp"

  describe "bracketMember" $ do
    it "parses simple bracket" $ do
      prop <- testParser bracketMember "['someProp']"
      prop `shouldBe` MBracketAccess (EStringLit "someProp")

    it "parses number lit bracket" $ do
      prop <- testParser bracketMember "[5]"
      prop `shouldBe` MBracketAccess (ENumberLit 5)

  describe "stringLitExpr" $ do
    it "parses double quoted" $ do
      slit <- testParser stringLitExpr "\"hello world!\""
      slit `shouldBe` EStringLit "hello world!"

    it "parses single quoted" $ do
      slit <- testParser stringLitExpr "'hello world!'"
      slit `shouldBe` EStringLit "hello world!"
